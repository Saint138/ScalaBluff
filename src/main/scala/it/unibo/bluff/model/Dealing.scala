package it.unibo.bluff.model

/** Utilities for initial distribution of cards to players. */
object Dealing:

  /** Distribute all cards round-robin. Returns (hands map, leftover deck). */
  def dealAll(players: List[PlayerId], deck: Deck): (Map[PlayerId, Hand], Deck) =
    require(players.nonEmpty, "Need at least one player")
    @annotation.tailrec
    def loop(current: Int, d: Deck, acc: Map[PlayerId, Hand]): (Map[PlayerId, Hand], Deck) =
      if d.isEmpty then (acc, d)
      else
        val (card :: Nil, d2) = d.draw(1): @unchecked
        val pid = players(current)
        val updatedHand = acc.getOrElse(pid, Hand.empty).add(card)
        loop((current + 1) % players.size, d2, acc.updated(pid, updatedHand))
    loop(0, deck, Map.empty)

  /**distribute and return a list aligned with players order. */
  def dealAllOrdered(players: List[PlayerId], deck: Deck): (List[Hand], Deck) =
    val (m, d2) = dealAll(players, deck)
    (players.map(m), d2)

