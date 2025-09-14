package it.unibo.bluff.model.cards

import it.unibo.bluff.model.cards.{Card, Rank, Suit}

/** Carta standard da poker: rango + seme */
final case class Card(rank: Rank, suit: Suit)

object Card:
  /** Tutte le 52 carte di un mazzo standard */
  val fullDeck: List[Card] =
    for
      suit <- Suit.values.toList
      rank <- Rank.order.toList
    yield Card(rank, suit)