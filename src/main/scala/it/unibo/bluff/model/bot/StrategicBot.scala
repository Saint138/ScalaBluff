package it.unibo.bluff.model.bot

import it.unibo.bluff.model.PlayerId
import it.unibo.bluff.model.cards.Rank
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.model.core.engine.Engine.GameCommand.{CallBluff, Play}
import it.unibo.bluff.model.core.state.GameState

import scala.util.Random

class StrategicBot(val id: PlayerId) extends Bot:
  private val rng = new Random()

  def decideMove(state: GameState): GameCommand =
    if shouldCallBluff(state) then
      CallBluff(id)
    else
      choosePlay(state)

  /** Strategia per la giocata */
  private def choosePlay(state: GameState): Play =
    val hand = state.hands(id).cards
    val possibleRank = state.fixedDeclaredRank.getOrElse {
      val ranksInGame: Set[Rank] = state.hands.values.flatMap(_.cards.map(_.rank)).toSet ++
        state.pile.allCards.map(_.rank)
      rng.shuffle(ranksInGame.toList).head
    }
    val matchingCards = hand.filter(_.rank == possibleRank)

    val chosenCards =
      if matchingCards.nonEmpty && rng.nextDouble() > 0.2 then
        // 80% → gioca coerente con il rank
        rng.shuffle(matchingCards).take(1 + rng.nextInt(matchingCards.size))
      else
        // 20% → bluffa anche se ha carte giuste
        rng.shuffle(hand).take(1)

    Play(id, chosenCards, possibleRank)

  /** Strategia per chiamare bluff */
  private def shouldCallBluff(state: GameState): Boolean =
    state.lastDeclaration match
      case Some(decl) =>
        val rankPlayed = decl.declared
        val cardsShown = decl.hiddenCards.size

        val alreadyOnTable = state.pile.allCards.count(_.rank == rankPlayed)
        val totalPossible = 4
        val suspicious = alreadyOnTable + cardsShown > totalPossible

        val myHandSize = state.hands(id).cards.size

        // 70% delle volte chiama se sospetto, 10% random anche senza sospetto
        (suspicious && rng.nextDouble() < 0.7) ||
          (rng.nextDouble() < 0.1 && myHandSize > 3)
      case None =>
        false


