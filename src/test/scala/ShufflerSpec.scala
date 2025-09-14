import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.model.*
import it.unibo.bluff.model.cards.{Card, Shuffler}

final class ShufflerSpec extends AnyFunSuite {

  test("Deterministic shuffler with same seed produces same order") {
    val cards = Card.fullDeck
    val s1 = Shuffler.deterministic(42L)
    val s2 = Shuffler.deterministic(42L)

    val d1 = s1.shuffle(cards)
    val d2 = s2.shuffle(cards)

    assert(d1 == d2, "Deterministic shuffle with same seed must be identical")
  }

  test("Deterministic shuffler with different seeds usually differs") {
    val cards = Card.fullDeck
    val d1 = Shuffler.deterministic(1L).shuffle(cards)
    val d2 = Shuffler.deterministic(2L).shuffle(cards)
    assert(d1 != d2, "Different seeds typically produce different permutations")
  }

  test("Random shuffler preserves card multiplicity and size") {
    val cards = Card.fullDeck
    val shuffled = Shuffler.random.shuffle(cards)
    assert(shuffled.size == cards.size)
    assert(shuffled.distinct.size == cards.distinct.size)
  }
}
