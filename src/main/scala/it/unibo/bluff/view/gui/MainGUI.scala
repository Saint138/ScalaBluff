package gui

import java.util.concurrent.atomic.AtomicReference

import it.unibo.bluff.view.gui.{MainMenuView, NewGameDialog, GameView}
import it.unibo.bluff.model.core.setup.GameSetup
import it.unibo.bluff.model.core.timer.GameTimer
import it.unibo.bluff.controller.GameController
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.core.state.{GameState, GameClocks}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.stats.{MatchStats, PlayerStats, StatsUpdater}
import it.unibo.bluff.model.bot.{RandomBot, BotRunner} // BOT

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.stage.StageStyle
import scalafx.scene.control.Alert
import scalafx.Includes.*
import it.unibo.bluff.view.gui.RulesDialog

// =============================
// Main GUI con due modalità chiare:
// - startMultiplayer
// - startVsBot
// =============================
object MainGUI extends JFXApp3:

  // --- Stato condiviso / controller / timer ---
  private val stateRef = new AtomicReference[GameState]()
  private val game     = new GameController()
  private var timer: Option[GameTimer] = None

  // --- Torneo ---
  private var tournamentRounds: Int = 1
  private var currentRound:    Int  = 1
  private var playerNames: Vector[String] = Vector.empty
  private var roundHandled: Boolean = false
  private var cumulativeStats: MatchStats = MatchStats(Map.empty)

  // --- Modalità ---
  private var vsBot: Boolean = false                 // TRUE => vs Bot, FALSE => Multiplayer
  private var botRunner: Option[BotRunner] = None
  private def stopTimer(): Unit =
    timer.foreach(_.stop())
    timer = None

  private def startTimer(tickMs: Long = 200L): Unit =
    stopTimer()
    val t = new GameTimer(stateRef, tickMillis = tickMs, onTimeout = { pid =>
      given it.unibo.bluff.model.TurnOrder = summon[it.unibo.bluff.model.TurnOrder]
      dispatch(GameCommand.Timeout(pid)) // via controller (statistiche incluse)
      checkRoundEnd()
    })
    timer = Some(t); t.start()

  private def stopBot(): Unit =
    botRunner.foreach(_.stop())
    botRunner = None

  /** Avvia il BotRunner se siamo in modalità vsBot (sincronizza anche il Controller). */
  private def startBotIfNeeded(st: GameState): Unit =
    stopBot()
    if vsBot then
      val botId =
        st.players.find(pid => st.nameOf(pid).equalsIgnoreCase("bot"))
          .getOrElse(st.players.last) // fallback: ultimo giocatore
      val bot = RandomBot(botId)

      // callback: allinea il Controller allo stato nuovo del bot PRIMA di aggiornare stateRef
      val onNewState: GameState => Unit = s => {
        game.setGameState(s)
        game.currentState.foreach(stateRef.set)
      }

      val runner = new BotRunner(
        stateRef   = stateRef,
        bot        = bot,
        pollMillis = 250L,
        onNewState = onNewState
      )
      botRunner = Some(runner)
      runner.start()

  private def dispatch(cmd: GameCommand) =
    val res = game.handleCommand(cmd)
    game.currentState.foreach(stateRef.set)
    res

  private def showStatsDialog(hdrText: String, content: String): Unit =
    StatsDialog.show(hdrText, content)

  private def prettyCumulative(gs: GameState, ms: MatchStats): String =
    val items = gs.players.map(pid => pid -> ms.perPlayer.getOrElse(pid, PlayerStats.empty))
    val sorted = items.sortBy { case (_, s) => (-s.wins, -(s.successfulCalls + s.successfulBluffs), -s.plays) }
    val lines = sorted.zipWithIndex.map { case ((pid, s), i) =>
      val name = gs.nameOf(pid)
      f"${i+1}%2d) $name%-15s  vittorie:${s.wins}%d  accuse-ok:${s.successfulCalls}%d  bluff-ok:${s.successfulBluffs}%d  giocate:${s.plays}%d  pile:${s.pileCardsTaken}%d  to:${s.timeouts}%d"
    }
    ("Classifica/Statistiche cumulative:\n" + lines.mkString("\n")).trim

  private def startRound(): Unit =
    roundHandled = false

    val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
    val stWithClocks = GameClocks.withClocks(stDealt, 60_000L)
    game.setGameState(stWithClocks)
    game.currentState.foreach(stateRef.set)
    startTimer(200L)
    startBotIfNeeded(stWithClocks) // ⬅️ parte il bot solo in modalità vsBot

    val mode   = if vsBot then " – VS Bot" else " – Multiplayer"
    val suffix = if tournamentRounds > 1 then s" (Round $currentRound/$tournamentRounds)" else ""
    stage.title = s"ScalaBluff$mode$suffix"

    stage.scene().root = new BorderPane:
      center = GameView(
        stateRef       = stateRef,
        maxPerTurnMs   = 60_000L,
        dispatch       = dispatch,
        onGameEnded    = _ => checkRoundEnd(),
        onExitToMenu   = () => {
          stopTimer()
          stopBot()
          stage.scene().root = MainMenuView(
            onNewGame = () => onNewGame(),
            onRules   = () => onRules()
          )
        },
        // Pausa/riprendi il timer quando l’overlay privacy è visibile
        onOverlayChange = visible => {
          if visible then stopTimer() else startTimer(200L)
        }
      )

  /** Se il round è finito, mostra stats e passa al prossimo / chiude il torneo. */
  private def checkRoundEnd(): Unit =
    if roundHandled then return
    game.currentState.foreach { st =>
      val winnerOpt = st.hands.collectFirst { case (pid, hand) if hand.size == 0 => pid }
      winnerOpt.foreach { _ =>
        roundHandled = true
        // ferma timer di partita e bot non appena il round è concluso
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
          startRound()
        else
          stopTimer()
          stopBot() // ⬅️ ferma il bot a fine torneo
          showStatsDialog("Torneo concluso", prettyCumulative(st, cumulativeStats))
          stage.scene().root = MainMenuView(
            onNewGame = () => onNewGame(),
            onRules   = () => onRules()
          )
      }
    }

  private def startMultiplayer(names: Vector[String], rounds: Int): Unit =
    vsBot = false
    playerNames      = names
    tournamentRounds = rounds.max(1)
    currentRound     = 1
    cumulativeStats  = MatchStats.empty((0 until names.size).map(PlayerId.apply))
    startRound()

  private def startVsBot(names: Vector[String], rounds: Int): Unit =
    vsBot = true
    val normalized =
      if names.exists(_.equalsIgnoreCase("Bot")) then names
      else if names.size == 1 then names :+ "Bot"
      else names.take(1) :+ "Bot"
    playerNames      = normalized
    tournamentRounds = rounds.max(1)
    currentRound     = 1
    cumulativeStats  = MatchStats.empty((0 until normalized.size).map(PlayerId.apply))
    startRound()

  private def onNewGame(): Unit =
    NewGameDialog.askPlayers().foreach { case (isSingle, names, rounds) =>
      if isSingle then startVsBot(names, rounds)       // VS Bot
      else            startMultiplayer(names, rounds)  // Multiplayer
    }

  private def onRules(): Unit =
    RulesDialog.show(() =>
      stage.scene().root = MainMenuView(
        onNewGame = () => onNewGame(),
        onRules   = () => onRules()
      )
    )

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 1100
      height = 720
      scene = new Scene(width.value, height.value):
        root = MainMenuView(
          onNewGame = () => onNewGame(),
          onRules   = () => onRules()
        )
    stage.centerOnScreen()
    stage.onCloseRequest = _ => {
      stopTimer()
      stopBot()
    }
