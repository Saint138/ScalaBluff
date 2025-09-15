package it.unibo.bluff.model.bot

import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.engine.Engine.GameCommand.{CallBluff, Play}
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.model.cards.Rank
import it.unibo.bluff.model.core.state.GameState

import scala.util.Random

class RandomBot(val id: PlayerId)extends Bot:
  private val rng = new Random()

  def decideMove(state: GameState): GameCommand =
    if rng.nextDouble() < 0.4 && canCallBluff(state) then
      callBluff(state)
    else
      play(state)

  /** Gioca da 1 a 3 carte casuali coerenti con il rank dichiarato o scelto casualmente tra le carte in mano */
  private def play(state: GameState): Play =
    val hand = state.hands(id).cards
    if hand.isEmpty then
      Play(id, Nil, Rank.Asso) // fallback
    else
      val declared = state.fixedDeclaredRank match
        case Some(rk) if hand.exists(_.rank == rk) => rk
        case _ => rng.shuffle(hand.map(_.rank)).headOption.getOrElse(Rank.Asso)

      val matchingCards = hand.filter(_.rank == declared)
      val maxCards = math.min(3, matchingCards.size)
      val numCards = if maxCards > 0 then 1 + rng.nextInt(maxCards) else 1
      val chosenCards = rng.shuffle(matchingCards).take(numCards)

      Play(id, chosenCards, declared)


  /** Chiamata bluff */
  private def callBluff(state: GameState): CallBluff =
    CallBluff(id)

  /** Controlla se il bot può chiamare bluff (es. c'è qualcosa sul tavolo) */
  private def canCallBluff(state: GameState): Boolean =
    state.pile.allCards.nonEmpty
