package it.unibo.bluff.model

opaque type PlayerId = Int
object PlayerId:
  def apply(i:Int): PlayerId = i
  extension (p:PlayerId) def value: Int = p

final case class Hand(cards: List[Card]):
  def size: Int = cards.size

  def remove(cs: List[Card]): Either[String, Hand] =
    if cs.forall(cards.contains) then
      // rimuove una occorrenza per carta
      val remaining = cs.foldLeft(cards){ (lst,c) =>
        val (before, after) = lst.span(_ != c)
        before ::: after.drop(1)
      }
      Right(Hand(remaining))
    else Left("Carte non presenti nella mano")

  def add(card: Card): Hand = Hand(card :: cards)
  def addAll(cs: List[Card]): Hand = Hand(cs ::: cards)

object Hand:
  val empty: Hand = Hand(Nil)
