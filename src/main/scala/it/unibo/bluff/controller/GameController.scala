package it.unibo.bluff.controller

import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.stats.{MatchStats, StatsUpdater}

/** Controller: mantiene lo stato della partita e aggiorna le statistiche. */
final class GameController:

  private var state: Option[GameState] = None
  private var stats: Option[MatchStats] = None

  def currentState: Option[GameState] = state
  def currentMatchStats: Option[MatchStats] = stats

  def setInitialState(st: GameState): Unit =
    state = Some(st)
    stats = Some(MatchStats.empty(st.players))

  def setCurrentState(st: GameState): Unit =
    state = Some(st)


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

  def botTurn(): Either[String, List[GameEvent]] = Right(Nil)


  def renderEvent(ev: GameEvent, st: GameState): List[String] = ev match
    case Engine.GameEvent.Dealt(sizes) =>
      sizes.map { case (p, sz) => s"${st.nameOf(p)} riceve $sz carte" }.toList
    case Engine.GameEvent.Played(player, declared, count) =>
      List(s"${st.nameOf(player)} dichiara $declared e gioca $count carte")
    case Engine.GameEvent.BotPlayed(player, declared, count) =>
      List(s"(BOT) ${st.nameOf(player)} dichiara $declared e gioca $count carte")
    case Engine.GameEvent.BluffCalled(by, against, truthful) =>
      val result = if truthful then "VERA" else "FALSA"
      List(s"${st.nameOf(by)} accusa ${st.nameOf(against.player)} → $result")
    case Engine.GameEvent.TimerExpired(player) =>
      List(s"Timeout: ${st.nameOf(player)} ha esaurito il tempo.")
    case Engine.GameEvent.QuartetCleared(player, rank, count) =>
      List(s"${st.nameOf(player)} elimina automaticamente $count carte ($rank)")
    case Engine.GameEvent.GameEnded(winner) =>
      List(s"🏆 Vince ${st.nameOf(winner)}!")
    case _ => Nil
