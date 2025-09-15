package it.unibo.bluff.controller

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.{GameClocks, GameState}
import it.unibo.bluff.model.core.setup.GameSetup
import it.unibo.bluff.model.stats.{MatchStats, PlayerStats, StatsUpdater}

import scala.language.postfixOps

/**
 * Responsabile della gestione dei round e delle statistiche cumulative.
 */
class RoundManager(
                    game: GameController,
                    stateRef: AtomicReference[GameState]
                  ):

  private var tournamentRounds: Int = 1
  private var currentRound: Int     = 1
  private var playerNames: Vector[String] = Vector.empty
  private var roundHandled: Boolean = false
  private var cumulativeStats: MatchStats = MatchStats(Map.empty)

  /** Callback invocata quando un round è concluso. */
  var onRoundEnd: (GameState, MatchStats, Boolean) => Unit = (_, _, _) => ()

  /** Callback invocata quando l’intero torneo è concluso. */
  var onTournamentEnd: (GameState, MatchStats) => Unit = (_, _) => ()

  /** Configura un nuovo torneo. */
  def initTournament(names: Vector[String], rounds: Int): Unit =
    playerNames = names
    tournamentRounds = rounds.max(1)
    currentRound = 1
    cumulativeStats = MatchStats.empty(names.indices.map(PlayerId.apply))

  /** Avvia un nuovo round. */
  def startRound(): GameState =
    roundHandled = false
    val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
    val stWithClocks    = GameClocks.withClocks(stDealt, 60_000L)
    game.setInitialState(stWithClocks)
    game.currentState.foreach(stateRef.set)
    stWithClocks

  /** Controlla se il round è concluso e gestisce aggiornamento statistiche. */
  def checkRoundEnd(): Unit =
    if roundHandled then return
    game.currentState.foreach { st =>
      val winnerOpt = st.hands.collectFirst { case (pid, hand) if hand.size == 0 => pid }
      winnerOpt.foreach { _ =>
        roundHandled = true
        val roundStats = game.currentMatchStats.getOrElse(MatchStats.empty(st.players))
        cumulativeStats =
          if cumulativeStats.perPlayer.isEmpty then roundStats
          else cumulativeStats.merge(roundStats)

        if currentRound < tournamentRounds then
          onRoundEnd(st, roundStats, true)
          currentRound += 1
        else
          onTournamentEnd(st, cumulativeStats)
      }
    }

  /** Genera una classifica leggibile. */
  def prettyCumulative(gs: GameState): String =
    val items = gs.players.map(pid => pid -> cumulativeStats.perPlayer.getOrElse(pid, PlayerStats.empty))
    val sorted = items.sortBy { case (_, s) =>
      (-s.wins, -(s.successfulCalls + s.successfulBluffs), -s.plays)
    }
    val lines = sorted.zipWithIndex.map { case ((pid, s), i) =>
      val name = gs.nameOf(pid)
      f"${i+1}%2d) $name%-15s  vittorie:${s.wins}%d  accuse-ok:${s.successfulCalls}%d  bluff-ok:${s.successfulBluffs}%d  giocate:${s.plays}%d  pile:${s.pileCardsTaken}%d  to:${s.timeouts}%d"
    }
    ("Classifica/Statistiche cumulative:\n" + lines.mkString("\n")).trim
