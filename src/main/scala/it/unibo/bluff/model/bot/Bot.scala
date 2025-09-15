package it.unibo.bluff.model.bot

import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.model.PlayerId

trait Bot {
  def id: PlayerId
  def decideMove(state: GameState): GameCommand
}
