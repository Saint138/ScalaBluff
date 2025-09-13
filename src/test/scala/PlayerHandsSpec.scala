import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.model.*

final class PlayerHandsSpec extends AnyFunSuite {

  test("Hand add and size behave correctly") {
    val c1 = Card(Rank.Due, Suit.Hearts)
    val c2 = Card(Rank.Tre, Suit.Spades)
    val h0 = Hand.empty
    val h1 = h0.add(c1)
    val h2 = h1.addAll(List(c2))

    assert(h1.size == 1)
    assert(h2.size == 2)
  }

  test("Hand remove removes only specified occurrences and returns error if missing") {
    val c1 = Card(Rank.Due, Suit.Hearts)
    val c2 = Card(Rank.Tre, Suit.Spades)
    val h = Hand(List(c1, c2, c1)) // two c1, one c2

    val removed = h.remove(List(c1))
    assert(removed.isRight)
    val hAfter = removed.toOption.get
    assert(hAfter.size == 2)
    assert(hAfter.cards.count(_ == c1) == 1)

    val bad = h.remove(List(Card(Rank.Asso, Suit.Diamonds)))
    assert(bad.isLeft)
  }
}
