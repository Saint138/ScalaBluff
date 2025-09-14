package gui

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.*
import it.unibo.bluff.model.bot.{RandomBot, BotRunner}
import it.unibo.bluff.model.core.state.{GameState, GameClocks}
import it.unibo.bluff.model.core.setup.GameSetup
import it.unibo.bluff.model.core.timer.GameTimer
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.controller.GameController
import it.unibo.bluff.model.stats.{MatchStats, PlayerStats, StatsUpdater}
import it.unibo.bluff.view.gui.{GameView, MainMenuView, NewGameDialog, RulesDialog}
import scalafx.scene.layout.BorderPane
import scalafx.scene.control.Alert
import scalafx.stage.Stage
import scalafx.Includes.jfxScene2sfx


object GUIController {

  // --- Stato condiviso ---
  private val stateRef = new AtomicReference[GameState]()
  private val game     = new GameController()
  private var timer: Option[GameTimer] = None

  private var tournamentRounds: Int = 1
  private var currentRound: Int = 1
  private var playerNames: Vector[String] = Vector.empty
  private var roundHandled: Boolean = false
  private var cumulativeStats: MatchStats = MatchStats(Map.empty)

  private var vsBot: Boolean = false
  private var botRunner: Option[BotRunner] = None

  // ---------------- Timer & Bot ----------------
  def stopTimer(): Unit =
    timer.foreach(_.stop())
    timer = None

  def startTimer(tickMs: Long = 200L): Unit =
    stopTimer()
    val t = new GameTimer(stateRef, tickMillis = tickMs, onTimeout = pid => {
      given it.unibo.bluff.model.TurnOrder = summon
      dispatch(GameCommand.Timeout(pid))
      checkRoundEnd(stage = null) // stage verrà passato dall’esterno se serve
    })
    timer = Some(t)
    t.start()

  def stopBot(): Unit =
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
        onGameEnded = () => checkRoundEnd(stage)
      )
      botRunner = Some(runner)
      runner.start()

  // ---------------- Comandi ----------------
  private def dispatch(cmd: GameCommand) =
    val res = game.handleCommand(cmd)
    game.currentState.foreach(stateRef.set)
    res

  // ---------------- GUI ----------------
  private def showStatsDialog(header: String, content: String): Unit =
    StatsDialog.show(header, content)

  private def prettyCumulative(gs: GameState, ms: MatchStats): String =
    val items = gs.players.map(pid => pid -> ms.perPlayer.getOrElse(pid, PlayerStats.empty))
    val sorted = items.sortBy { case (_, s) => (-s.wins, -(s.successfulCalls + s.successfulBluffs), -s.plays) }
    val lines = sorted.zipWithIndex.map { case ((pid, s), i) =>
      val name = gs.nameOf(pid)
      f"${i+1}%2d) $name%-15s  vittorie:${s.wins}%d  accuse-ok:${s.successfulCalls}%d  bluff-ok:${s.successfulBluffs}%d  giocate:${s.plays}%d  pile:${s.pileCardsTaken}%d  to:${s.timeouts}%d"
    }
    ("Classifica/Statistiche cumulative:\n" + lines.mkString("\n")).trim

  // ---------------- Round ----------------
  private def startRound(stage: Stage): Unit =
    roundHandled = false
    val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
    val stWithClocks = GameClocks.withClocks(stDealt, 60_000L)
    game.setGameState(stWithClocks)
    game.currentState.foreach(stateRef.set)
    startTimer(200L)
    startBotIfNeeded(stWithClocks, stage)

    stage.scene().root = new BorderPane {
      center = GameView(
        stateRef = stateRef,
        maxPerTurnMs = 60_000L,
        dispatch = dispatch,
        onGameEnded = _ => checkRoundEnd(stage),
        onExitToMenu = () => {
          stopTimer()
          stopBot()
          stage.scene().root = MainMenuView(
            onNewGame = () => onNewGame(stage),
            onRules = () => onRules(stage)
          )
        },
        onOverlayChange = visible => if visible then stopTimer() else startTimer(200L)
      )
    }

  def checkRoundEnd(stage: Stage): Unit =
    if roundHandled then return
    game.currentState.foreach { st =>
      val winnerOpt = st.hands.collectFirst { case (pid, hand) if hand.size == 0 => pid }
      winnerOpt.foreach { _ =>
        roundHandled = true
        stopTimer()
        stopBot()
        val roundStats = game.currentMatchStats.getOrElse(MatchStats.empty(st.players))
        showStatsDialog(s"Round $currentRound concluso", StatsUpdater.pretty(st, roundStats))

        cumulativeStats =
          if cumulativeStats.perPlayer.isEmpty then roundStats
          else cumulativeStats.merge(roundStats)

        if currentRound < tournamentRounds then
          currentRound += 1
          new Alert(Alert.AlertType.Information) {
            headerText = s"Preparazione round $currentRound / $tournamentRounds"
            contentText = "Si rimescola e si riparte."
          }.showAndWait()
          startRound(stage)
        else
          showStatsDialog("Torneo concluso", prettyCumulative(st, cumulativeStats))
          stage.scene().root = MainMenuView(
            onNewGame = () => onNewGame(stage),
            onRules = () => onRules(stage)
          )
      }
    }

  // ---------------- Avvio ----------------
  def startMultiplayer(names: Vector[String], rounds: Int, stage: Stage): Unit =
    vsBot = false
    playerNames = names
    tournamentRounds = rounds.max(1)
    currentRound = 1
    cumulativeStats = MatchStats.empty(names.indices.map(PlayerId.apply))
    startRound(stage)

  def startVsBot(names: Vector[String], rounds: Int, stage: Stage): Unit =
    vsBot = true
    val normalized =
      if names.exists(_.equalsIgnoreCase("Bot")) then names
      else if names.size == 1 then names :+ "Bot"
      else names.take(1) :+ "Bot"
    playerNames = normalized
    tournamentRounds = rounds.max(1)
    currentRound = 1
    cumulativeStats = MatchStats.empty(normalized.indices.map(PlayerId.apply))
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
        onRules = () => onRules(stage)
      )
    )
}
