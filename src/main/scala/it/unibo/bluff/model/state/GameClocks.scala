package it.unibo.bluff.model.state

import it.unibo.bluff.model.*

/** Helper object that centralizes clock operations for GameState. */
object GameClocks:

  /** Initialize all players' clocks to the given millis. */
  def withClocks(gs: GameState, initialMillis: Long): GameState =
    gs.copy(clocks = gs.players.map(_ -> initialMillis).toMap)

  /** Decrease the clock of a player by deltaMillis (non-negative). Clocks floor at 0. */
  def tickClock(gs: GameState, player: PlayerId, deltaMillis: Long): GameState =
    val old = gs.clocks.getOrElse(player, 0L)
    val next = math.max(0L, old - math.max(0L, deltaMillis))
    gs.copy(clocks = gs.clocks.updated(player, next))

  /** Set the clock for a specific player. */
  def setClock(gs: GameState, player: PlayerId, valueMillis: Long): GameState =
    gs.copy(clocks = gs.clocks.updated(player, math.max(0L, valueMillis)))
