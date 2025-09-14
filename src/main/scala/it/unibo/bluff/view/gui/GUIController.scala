package gui

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.*
import it.unibo.bluff.model.bot.{RandomBot, BotRunner}
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.timer.GameTimer
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.controller.{GameController, RoundManager}
import it.unibo.bluff.view.gui.{GameView, MainMenuView, NewGameDialog, RulesDialog}
import scalafx.scene.layout.BorderPane
import scalafx.scene.control.Alert
import scalafx.stage.Stage
import scalafx.Includes.jfxScene2sfx

object GUIController:

  private val stateRef = new AtomicReference[GameState]()
  private val game     = new GameController()
  private val roundMgr = new RoundManager(game, stateRef)

  private var timer: Option[GameTimer]   = None
  private var botRunner: Option[BotRunner] = None
  private var vsBot: Boolean = false

  // ---------------- Timer & Bot ----------------
  private def stopTimer(): Unit =
    timer.foreach(_.stop())
    timer = None

  private def startTimer(tickMs: Long = 200L, stage: Stage): Unit =
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

  private def startBotIfNeeded(st: GameState, stage: Stage): Unit =
    stopBot()
    if vsBot then
      val botId = st.players.find(pid => st.nameOf(pid).equalsIgnoreCase("Bot"))
        .getOrElse(st.players.last)
      val bot = RandomBot(botId)
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
    game.currentState.foreach(stateRef.set)
    res

  // ---------------- Avvio Round ----------------
  private def startRound(stage: Stage): Unit =
    val stWithClocks = roundMgr.startRound()
    startTimer(200L, stage)
    startBotIfNeeded(stWithClocks, stage)

    stage.scene().root = new BorderPane {
      center = GameView(
        stateRef = stateRef,
        maxPerTurnMs = 60_000L,
        dispatch = dispatch,
        onGameEnded = _ => roundMgr.checkRoundEnd(),
        onExitToMenu = () => {
          stopTimer()
          stopBot()
          stage.scene().root = MainMenuView(
            onNewGame = () => onNewGame(stage),
            onRules   = () => onRules(stage)
          )
        },
        onOverlayChange = visible => if visible then stopTimer() else startTimer(200L, stage)
      )
    }

  // ---------------- Collegamento RoundManager → GUI ----------------
  roundMgr.onRoundEnd = (st, roundStats, hasNext) => {
    StatsDialog.show(s"Round concluso", it.unibo.bluff.model.stats.StatsUpdater.pretty(st, roundStats))
    if hasNext then
      new Alert(Alert.AlertType.Information) {
        headerText = "Preparazione nuovo round"
        contentText = "Si rimescola e si riparte."
      }.showAndWait()
      startRound(stage = null) // iniettato al bisogno
  }

  roundMgr.onTournamentEnd = (st, cumulative) => {
    StatsDialog.show("Torneo concluso", roundMgr.prettyCumulative(st))
    stopTimer()
    stopBot()
  }

  // ---------------- API pubblica ----------------
  def startMultiplayer(names: Vector[String], rounds: Int, stage: Stage): Unit =
    vsBot = false
    roundMgr.initTournament(names, rounds)
    startRound(stage)

  def startVsBot(names: Vector[String], rounds: Int, stage: Stage): Unit =
    vsBot = true
    val normalized =
      if names.exists(_.equalsIgnoreCase("Bot")) then names
      else if names.size == 1 then names :+ "Bot"
      else names.take(1) :+ "Bot"
    roundMgr.initTournament(normalized, rounds)
    startRound(stage)

  def onNewGame(stage: Stage): Unit =
    NewGameDialog.askPlayers().foreach { case (isSingle, names, rounds) =>
      if isSingle then startVsBot(names, rounds, stage)
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

