package it.unibo.bluff.model.stats

import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.GameEvent

final case class PlayerStats(
  plays: Int = 0,
  cardsPlayed: Int = 0,
  calls: Int = 0,
  successfulCalls: Int = 0,   // accusa corretta (bluff scoperto)
  successfulBluffs: Int = 0,  // accusa sbagliata (bluff riuscito per il dichiarante)
  pileCardsTaken: Int = 0,    
  timeouts: Int = 0,
  wins: Int = 0
):
  def +(that: PlayerStats): PlayerStats =
    PlayerStats(
      plays + that.plays,
      cardsPlayed + that.cardsPlayed,
      calls + that.calls,
      successfulCalls + that.successfulCalls,
      successfulBluffs + that.successfulBluffs,
      pileCardsTaken + that.pileCardsTaken,
      timeouts + that.timeouts,
      wins + that.wins
    )

object PlayerStats:
  val empty: PlayerStats = PlayerStats()

final case class MatchStats(perPlayer: Map[PlayerId, PlayerStats]):
  def updated(pid: PlayerId, f: PlayerStats => PlayerStats): MatchStats =
    val cur = perPlayer.getOrElse(pid, PlayerStats.empty)
    copy(perPlayer = perPlayer.updated(pid, f(cur)))

  def merge(other: MatchStats): MatchStats =
    val ids = perPlayer.keySet ++ other.perPlayer.keySet
    MatchStats(ids.map { pid =>
      pid -> (perPlayer.getOrElse(pid, PlayerStats.empty) + other.perPlayer.getOrElse(pid, PlayerStats.empty))
    }.toMap)

object MatchStats:
  def empty(players: Iterable[PlayerId]): MatchStats =
    MatchStats(players.map(_ -> PlayerStats.empty).toMap)

object StatsUpdater:
  def apply(prev: GameState, events: List[GameEvent], next: GameState, cur: MatchStats): MatchStats =
    events.foldLeft(cur) { (acc, ev) =>
      ev match
        // Giocata umano
        case GameEvent.Played(p, _, count) =>
          acc.updated(p, s => s.copy(plays = s.plays + 1, cardsPlayed = s.cardsPlayed + count))

        // Giocata bot (conteggia come Played)
        case GameEvent.BotPlayed(p, _, count) =>
          acc.updated(p, s => s.copy(plays = s.plays + 1, cardsPlayed = s.cardsPlayed + count))

        case GameEvent.BluffCalled(by, against, truthful) =>
          val pileSize = prev.pile.allCards.size

          // 1) Aggiorna l'accusatore: ha fatto un'accusa; se la dichiarazione era falsa, l'accusa è riuscita
          val acc1 = acc.updated(by, s => s.copy(
            calls = s.calls + 1,
            successfulCalls = s.successfulCalls + (if truthful then 0 else 1)
          ))

          // 2) Chi prende la pila? (truthful ⇒ accusa sbagliata ⇒ carte all'accusatore; altrimenti al dichiarante)
          val receiver = if truthful then by else against.player
          val acc2 = acc1.updated(receiver, s => s.copy(
            pileCardsTaken = s.pileCardsTaken + pileSize
          ))

          // 3) Bluff riuscito = accusa sbagliata ⇒ attribuisco al dichiarante quando truthful == true
          val acc3 =
            if truthful then
              acc2.updated(against.player, s => s.copy(successfulBluffs = s.successfulBluffs + 1))
            else
              acc2

          acc3

        case GameEvent.TimerExpired(p) =>
          acc.updated(p, s => s.copy(timeouts = s.timeouts + 1))

        case GameEvent.GameEnded(w) =>
          acc.updated(w, s => s.copy(wins = s.wins + 1))

        case _ => acc
    }

  def pretty(gs: GameState, ms: MatchStats): String =
    val lines = gs.players.map { pid =>
      val nm = gs.nameOf(pid)
      val st = ms.perPlayer.getOrElse(pid, PlayerStats.empty)
      f"""- $nm:
         |    giocate: ${st.plays}%d  (carte: ${st.cardsPlayed}%d)
         |    accuse: ${st.calls}%d   | accuse riuscite: ${st.successfulCalls}%d
         |    bluff riusciti: ${st.successfulBluffs}%d
         |    carte dalla pila: ${st.pileCardsTaken}%d
         |    timeout: ${st.timeouts}%d
         |    vittorie: ${st.wins}%d
         |""".stripMargin
    }
    ("Statistiche giocatori:\n" + lines.mkString).trim
