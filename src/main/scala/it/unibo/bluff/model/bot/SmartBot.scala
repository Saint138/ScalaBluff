package it.unibo.bluff.model.bot

import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.*
import it.unibo.bluff.model.core.engine.Engine.GameCommand.{CallBluff, Play}
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.model.cards.Rank

import scala.util.Random

/*A SmartBot that decides moves using a mix of bluffing and strategic play based on hand, table state, and probabilities.*/

class SmartBot(val id: PlayerId) extends Bot:
  private val rng = new Random()

  def decideMove(state: GameState): GameCommand =
    if shouldCallBluff(state) then
      CallBluff(id)
    else
      choosePlay(state)

  /** Strategia per la giocata */
  private def choosePlay(state: GameState): Play =
    val hand = state.hands(id).cards
    val possibleRanksInGame = state.hands.values.flatMap(_.cards.map(_.rank)).toSet ++
      state.pile.allCards.map(_.rank)

    val rankToPlay =
      if state.fixedDeclaredRank.exists(possibleRanksInGame.contains) then state.fixedDeclaredRank.get
      else rng.shuffle(possibleRanksInGame.toList).head

    val cardsOfRank = hand.filter(_.rank == rankToPlay)
    val maxCardsToDeclare = math.min(cardsOfRank.size, 3) // mai dichiarare più carte di quante realmente ne possiede
    val fewCardsLeft = hand.size <= 3
    val bluffEarly = hand.size >= 5 && rng.nextDouble() < 0.7

    val chosenCards =
      if fewCardsLeft then
        rng.shuffle(cardsOfRank).take(maxCardsToDeclare)
      else if bluffEarly then
        rng.shuffle(hand).take(1 + rng.nextInt(math.min(3, hand.size)))
      else
        if cardsOfRank.nonEmpty && rng.nextDouble() < 0.8 then
          rng.shuffle(cardsOfRank).take(1 + rng.nextInt(maxCardsToDeclare))
        else
          rng.shuffle(hand).take(1 + rng.nextInt(math.min(3, hand.size)))

    Play(id, chosenCards, rankToPlay)

  private def shouldCallBluff(state: GameState): Boolean =
    state.lastDeclaration match
      case Some(decl) =>
        val declaredRank = decl.declared
        val declaredCount = decl.hiddenCards.size
        val alreadyOnTable = state.pile.allCards.count(_.rank == declaredRank)
        val totalPossible = 4
        val suspicious = alreadyOnTable + declaredCount > totalPossible

        val myHandSize = state.hands(id).cards.size

        val baseProb =
          if suspicious then 0.9 // se sospetto, chiama bluff 90% delle volte
          else if myHandSize <= 3 then 0.7 // poche carte in mano rischia di più
          else 0.3 // altrimenti chiama bluff con probabilità bassa

        rng.nextDouble() < baseProb
      case None => false
  

