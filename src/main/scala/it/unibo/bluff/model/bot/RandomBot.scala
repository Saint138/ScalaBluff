package it.unibo.bluff.model.bot

import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*
import it.unibo.bluff.engine.Engine.GameCommand.{CallBluff, Play}
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.TurnOrder.given

import scala.util.Random

class RandomBot(val id: PlayerId):
  private val rng = new Random()
  
  def decideMove(state: GameState): GameCommand =
    if rng.nextDouble() < 0.4 && canCallBluff(state) then
      callBluff(state)
    else
      play(state)

  /** Gioca da 1 a 3 carte casuali dalla mano */
  private def play(state: GameState): Play =
    val hand = state.hands(id).cards
    if hand.isEmpty then
      Play(id, Nil, Rank.Asso) // fallback
    else
      val maxCards = math.min(3, hand.size)
      val numCards = 1 + rng.nextInt(maxCards) // da 1 a maxCards
      val chosenCards = rng.shuffle(hand).take(numCards)

      val declared = state.fixedDeclaredRank.getOrElse(
        Rank.values(rng.nextInt(Rank.values.size))
      )

      Play(id, chosenCards, declared)

  /** Chiamata bluff */
  private def callBluff(state: GameState): CallBluff =
    CallBluff(id)

  /** Controlla se il bot può chiamare bluff (es. c'è qualcosa sul tavolo) */
  private def canCallBluff(state: GameState): Boolean =
    state.pile.allCards.size>0
