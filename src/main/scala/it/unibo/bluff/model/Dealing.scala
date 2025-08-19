package it.unibo.bluff.model

trait Dealing {
  def deal(deck: Deck, players: Int, cardsPerPlayer: Int): Map[Int, List[Card]]
}

object Dealing {
  def apply(): Dealing = new DealingImpl

  private class DealingImpl extends Dealing {
    def deal(deck: Deck, players: Int, cardsPerPlayer: Int): Map[Int, List[Card]] = {
      require(players > 0, "Number of players must be positive")
      require(cardsPerPlayer > 0, "Number of cards per player must be positive")
      require(deck.size >= players * cardsPerPlayer, "Not enough cards in the deck")

      val (hands, remainingDeck) = dealHands(deck, players, cardsPerPlayer)
      hands
    }

    private def dealHands(deck: Deck, players: Int, cardsPerPlayer: Int): (Map[Int, List[Card]], Deck) = {
      var currentDeck = deck
      val hands = (1 to players).map { player =>
        val (cards, newDeck) = currentDeck.draw(cardsPerPlayer)
        currentDeck = newDeck
        player -> cards
      }.toMap
      (hands, currentDeck)
    }
  }
}
