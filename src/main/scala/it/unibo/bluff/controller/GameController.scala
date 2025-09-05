package it.unibo.bluff.controller

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.stats.{MatchStats, StatsUpdater}

/** Controller MVC: mantiene lo stato della partita e aggiorna le statistiche. */
final class GameController:

  private var state: Option[GameState] = None
  private var stats: Option[MatchStats] = None

  def currentState: Option[GameState] = state
  def currentMatchStats: Option[MatchStats] = stats

  /** Sostituisce lo stato corrente e resetta le statistiche di round. */
  def setGameState(st: GameState): Unit =
    state = Some(st)
    stats = Some(MatchStats.empty(st.players))

  /** Applica un comando al motore aggiornando stato e statistiche. */
  def handleCommand(cmd: GameCommand): Either[String, List[GameEvent]] =
    state match
      case None => Left("Nessuna partita in corso")
      case Some(st) =>
        val prev = st
        Engine.step(st, cmd).map { case (st2, evs) =>
          state = Some(st2)
          stats = stats.map(ms => StatsUpdater(prev, evs, st2, ms))
          evs
        }

  /** No-op se non usi bot; tienilo per compatibilità chiamanti. */
  def botTurn(): Either[String, List[GameEvent]] = Right(Nil)
