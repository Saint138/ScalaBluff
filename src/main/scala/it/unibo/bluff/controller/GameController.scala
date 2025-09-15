package it.unibo.bluff.controller

import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.stats.{MatchStats, StatsUpdater}

/** Controller MVC: mantiene lo stato della partita e aggiorna le statistiche. */
final class GameController:

  private var state: Option[GameState] = None
  private var stats: Option[MatchStats] = None

  def currentState: Option[GameState] = state
  def currentMatchStats: Option[MatchStats] = stats

  /** Inizializza lo stato a inizio round e resetta le stats. */
  def setInitialState(st: GameState): Unit =
    state = Some(st)
    stats = Some(MatchStats.empty(st.players))

  /** Aggiorna lo stato corrente senza toccare le stats (sync esterni, es. bot runner). */
  def setCurrentState(st: GameState): Unit =
    state = Some(st)

  /** Applica un comando al motore aggiornando stato e statistiche. */
  def handleCommand(cmd: GameCommand): Either[String, List[GameEvent]] =
    state match
      case None => Left("Nessuna partita in corso")
      case Some(st) =>
        val prev = st
        Engine.step(st, cmd).map { case (st2, evs) =>
          state = Some(st2)
          updateStats(prev, evs, st2)
          evs
        }

  private def updateStats(prev: GameState, evs: List[GameEvent], st2: GameState): Unit =
    stats = stats.map(ms => StatsUpdater(prev, evs, st2, ms))

  /** No-op se non usi bot; tienilo per compatibilità chiamanti. */
  def botTurn(): Either[String, List[GameEvent]] = Right(Nil)

  def renderEvent(ev: Engine.GameEvent, st: GameState): List[String] = ev match
    case Engine.GameEvent.Dealt(sz) =>
      sz.map { case (p, s) => s"Distribuite carte: ${st.nameOf(p)}=$s" }.toList
    case Engine.GameEvent.Played(p, d, c) =>
      List(s"${st.nameOf(p)} dichiara $d e gioca $c carte")
    case Engine.GameEvent.BotPlayed(p, d, c) =>
      List(s"(BOT) ${st.nameOf(p)} dichiara $d e gioca $c carte")
    case Engine.GameEvent.BluffCalled(by, ag, truth) =>
      List(s"${st.nameOf(by)} accusa ${st.nameOf(ag.player)} → " + (if truth then "VERA" else "FALSA"))
    case Engine.GameEvent.TimerExpired(p) =>
      List(s"Timeout: ${st.nameOf(p)} ha esaurito il tempo.")
    case Engine.GameEvent.QuartetCleared(p, r, cnt) =>
      List(s"♻️ ${st.nameOf(p)} elimina automaticamente $cnt carte ($r)")
    case Engine.GameEvent.GameEnded(w) =>
      List(s"🏆 Vince ${st.nameOf(w)}!")
    case _ => Nil
