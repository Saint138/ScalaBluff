package it.unibo.bluff.controller

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.*
import it.unibo.bluff.model.bot.{BotFactory, BotManager, BotRunner}
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.timer.GameTimer
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.view.gui.{GameView, MainMenuView, NewGameDialog, RulesDialog}
import scalafx.application.Platform
import it.unibo.bluff.view.gui.StatsDialog
import scalafx.scene.layout.BorderPane
import scalafx.scene.control.Alert
import scalafx.stage.Stage
import scalafx.Includes.jfxScene2sfx

object GUIController:

  private val stateRef = new AtomicReference[GameState]()
  private val game     = new GameController()
  private val roundMgr = new RoundManager(game, stateRef)
  private var botKind: String = "random"

  private var mainStage: Option[Stage] = None
  private var timer: Option[GameTimer]     = None
  private var botRunner: Option[BotRunner] = None
  private var vsBot: Boolean = false

  // --- Configurazione BotManager singleton ---
  BotManager.executeCommand = cmd =>
    game.handleCommand(cmd).map { evs =>
      val st2 = game.currentState.getOrElse(
        throw new IllegalStateException("No state available after command")
      )
      (st2, evs)
    }

  BotManager.onStateUpdate = s => {
    game.setCurrentState(s)
    game.currentState.foreach(stateRef.set)
  }

  // onEvents verrà impostato dalla GUI tramite subscribeToExternalEvents

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
        onNewState = s => { game.setCurrentState(s); game.currentState.foreach(stateRef.set) },
        onGameEnded = () => roundMgr.checkRoundEnd()
      )
      botRunner = Some(runner)
      runner.start()

  // ---------------- Comandi ----------------
  private def dispatch(cmd: GameCommand) =
    val res = game.handleCommand(cmd)
    game.currentState.foreach(stateRef.set)
    res

  // ---------------- Avvio / UI ----------------
  private def startRound(stage: Stage): Unit =
    mainStage = Some(stage)
    val stWithClocks = roundMgr.startRound()
    game.setInitialState(stWithClocks)
    startTimer(200L)
    startBotIfNeeded(stWithClocks)

    Platform.runLater {
      stage.scene().root = new BorderPane {
        center = GameView(
          stateRef = stateRef,
          maxPerTurnMs = 60_000L,
          dispatch = dispatch,
          renderEvent = game.renderEvent,
          subscribeToExternalEvents = cb => { BotManager.onEvents = cb },
          onGameEnded = _ => roundMgr.checkRoundEnd(),
          onExitToMenu = () => {
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

  private def startMultiplayer(names: Vector[String], rounds: Int, stage: Stage): Unit =
    vsBot = false
    roundMgr.initTournament(names, rounds)
    startRound(stage)

  private def startVsBot(names: Vector[String], rounds: Int, stage: Stage, kind: String): Unit =
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
    mainStage = None
