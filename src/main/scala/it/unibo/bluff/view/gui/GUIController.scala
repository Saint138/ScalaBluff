package gui

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.*
import it.unibo.bluff.model.bot.{BotFactory, BotManager, BotRunner}
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.timer.GameTimer
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.controller.{GameController, RoundManager}
import it.unibo.bluff.view.gui.{GameView, MainMenuView, NewGameDialog, RulesDialog}
import scalafx.application.Platform
import scalafx.scene.layout.BorderPane
import scalafx.scene.control.Alert
import scalafx.stage.Stage
import scalafx.Includes.jfxScene2sfx

object GUIController:

  // Stato condiviso
  private val stateRef = new AtomicReference[GameState]()
  private val game     = new GameController()
  private val roundMgr = new RoundManager(game, stateRef)
  private var botKind: String = "random"

  // Stage principale (iniettato quando si lancia una partita)
  private var mainStage: Option[Stage] = None

  // Timer / bot runner
  private var timer: Option[GameTimer]   = None
  private var botRunner: Option[BotRunner] = None
  private var vsBot: Boolean = false

  // --- Inizializza i collegamenti col BotManager (una sola volta) ---
  // Il Bot deve eseguire i comandi tramite il controller => passiamo direttamente la funzione handleCommand
  BotManager.executeCommand = cmd =>
    game.handleCommand(cmd).map { evs =>
      val st2 = game.currentState.getOrElse(
        throw new IllegalStateException("No state available after command")
      )
      (st2, evs)
    }

  // quando BotManager segnala uno stato nuovo, aggiorniamo controller + stateRef (utile per GUI)
  BotManager.onStateUpdate = s => { game.setGameState(s); game.currentState.foreach(stateRef.set) }
  // log eventi bot (la GUI si iscrive al BotManager.onEvents in GameView)
  BotManager.onEvents = evs => println("[BotManager] events: " + evs)

  // ---------------- Timer & Bot ----------------
  private def stopTimer(): Unit =
    timer.foreach(_.stop())
    timer = None

  private def startTimer(tickMs: Long = 200L): Unit =
    stopTimer()
    val t = new GameTimer(stateRef, tickMillis = tickMs, onTimeout = pid => {
      given it.unibo.bluff.model.TurnOrder = summon
      dispatch(GameCommand.Timeout(pid))
      roundMgr.checkRoundEnd()
    })
    timer = Some(t)
    t.start()

  private def stopBot(): Unit =
    botRunner.foreach(_.stop())
    botRunner = None

  private def startBotIfNeeded(st: GameState): Unit =
    stopBot()
    if vsBot then
      val botId = st.players.find(pid => st.nameOf(pid).equalsIgnoreCase("Bot")).getOrElse(st.players.last)
      val bot = BotFactory(botKind, botId)
      val runner = new BotRunner(
        stateRef   = stateRef,
        bot        = bot,
        pollMillis = 250L,
        onNewState = s => { game.setGameState(s); game.currentState.foreach(stateRef.set) },
        onGameEnded = () => roundMgr.checkRoundEnd()
      )
      botRunner = Some(runner)
      runner.start()

  // ---------------- Comandi ----------------
  private def dispatch(cmd: GameCommand) =
    val res = game.handleCommand(cmd)
    // sincronizza stateRef con il controller
    game.currentState.foreach(stateRef.set)
    res

  // ---------------- Avvio / UI ----------------
  /** Avvia un round e mostra GameView nello stage fornito. */
  private def startRound(stage: Stage): Unit =
    // salva lo stage (per poterlo riusare nei callback)
    mainStage = Some(stage)

    val stWithClocks = roundMgr.startRound()
    // avvia timer e bot (se necessario)
    startTimer(200L)
    startBotIfNeeded(stWithClocks)

    // imposta la view
    Platform.runLater {
      stage.scene().root = new BorderPane {
        center = GameView(
          stateRef = stateRef,
          maxPerTurnMs = 60_000L,
          dispatch = dispatch,
          onGameEnded = _ => roundMgr.checkRoundEnd(),
          onExitToMenu = () => {
            // chiusura partita: ferma timer e bot e torna al menu
            stopTimer()
            stopBot()
            mainStage.foreach { s =>
              s.scene().root = MainMenuView(
                onNewGame = () => onNewGame(s),
                onRules   = () => onRules(s)
              )
            }
          },
          onOverlayChange = visible => if visible then stopTimer() else startTimer(200L)
        )
      }
    }

  // ---------------- Collegamento RoundManager → GUI ----------------
  // Quando un round finisce: mostra stats e, se c'è un altro round, lo avvia
  roundMgr.onRoundEnd = (st, roundStats, hasNext) => Platform.runLater {
    StatsDialog.show(s"Round concluso", it.unibo.bluff.model.stats.StatsUpdater.pretty(st, roundStats))
    if hasNext then
      new Alert(Alert.AlertType.Information) {
        headerText = "Preparazione nuovo round"
        contentText = "Si rimescola e si riparte."
      }.showAndWait()
      mainStage match
        case Some(s) => startRound(s)
        case None    => println("[GUIController] mainStage not available to start next round")
  }

  // Quando tutto il torneo è finito: mostra stats cumulative e torna al menu principale
  roundMgr.onTournamentEnd = (st, cumulative) => Platform.runLater {
    StatsDialog.show("Torneo concluso", roundMgr.prettyCumulative(st))
    stopTimer()
    stopBot()
    mainStage.foreach { s =>
      s.scene().root = MainMenuView(
        onNewGame = () => onNewGame(s),
        onRules   = () => onRules(s)
      )
    }
  }

  // ---------------- API pubblica ----------------
  def startMultiplayer(names: Vector[String], rounds: Int, stage: Stage): Unit =
    vsBot = false
    roundMgr.initTournament(names, rounds)
    startRound(stage)

  def startVsBot(names: Vector[String], rounds: Int, stage: Stage, kind: String): Unit =
    vsBot = true
    botKind = kind
    val normalized =
      if names.exists(_.equalsIgnoreCase("Bot")) then names
      else if names.size == 1 then names :+ "Bot"
      else names.take(1) :+ "Bot"
    roundMgr.initTournament(normalized, rounds)
    startRound(stage)

  def onNewGame(stage: Stage): Unit =
    NewGameDialog.askPlayers().foreach { case (isSingle, names, rounds, chosenBotKind) =>
      if isSingle then startVsBot(names, rounds, stage, chosenBotKind)
      else startMultiplayer(names, rounds, stage)
    }

  def onRules(stage: Stage): Unit =
    RulesDialog.show(() =>
      stage.scene().root = MainMenuView(
        onNewGame = () => onNewGame(stage),
        onRules   = () => onRules(stage)
      )
    )

  def shutdown(): Unit =
    stopTimer()
    stopBot()
    // pulizia opzionale
    mainStage = None
