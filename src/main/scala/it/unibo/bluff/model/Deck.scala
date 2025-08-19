package it.unibo.bluff.model

trait Deck:
  def draw(n: Int): (List[Card], Deck)
  def size: Int
  def isEmpty: Boolean = size == 0

object Deck:
  def standard(shuffler: Shuffler): Deck =
    ListDeck(shuffler.shuffle(Card.fullDeck))
  def standardShuffled(seed : Long) : Deck =
    standard(Shuffler.deterministic(seed))
final case class ListDeck(private val cards: List[Card]) extends Deck:
  def draw(n: Int): (List[Card], Deck) =
    val (taken, rest) = cards.splitAt(n)
    (taken, ListDeck(rest))
  def size: Int = cards.size
  override def toString(): String = s"Deck($size cards)"