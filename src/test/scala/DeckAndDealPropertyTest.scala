package it.unibo.bluff.model.state
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.*
import org.scalacheck.Gen
import it.unibo.bluff.model.Deck
import it.unibo.bluff.model.PlayerId
import it.unibo.bluff.model.Dealing

class DeckAndDealingPropertySpec extends AnyFunSuite:

  test("Deterministic shuffle same seed yields identical order") {
    val d1 = Deck.standardShuffled(42L)
    val d2 = Deck.standardShuffled(42L)
    assert(d1 == d2)
  }

  test("Different seeds usually produce different permutation") {
    val d1 = Deck.standardShuffled(1L)
    val d2 = Deck.standardShuffled(2L)
    assert(d1 != d2)
  }

  test("DealAll preserves all cards, no duplicates, balanced sizes") {
    val playerCounts = Gen.chooseNum(1, 8)
    forAll(playerCounts) { nPlayers =>
      val players = (1 to nPlayers).toList.map(i => PlayerId(i))
      val deck = Deck.standardShuffled(123L)
      val (hands, leftover) = Dealing.dealAll(players, deck)

      val allDealt = hands.values.flatMap(_.cards).toList
      assert(allDealt.distinct.size == allDealt.size)
      assert(allDealt.size == 52)        // tutte le carte distribuite
      assert(leftover.size == 0)         // nessun resto

      val sizes = players.map(p => hands(p).cards.size)
      assert(sizes.max - sizes.min <= 1) // bilanciamento (alcuni possono avere una carta in più)
    }
  }