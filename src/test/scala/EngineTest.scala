import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*
import it.unibo.bluff.model.util.RNG

// given TurnOrder in scope
import it.unibo.bluff.model.TurnOrder.given

final class EngineTest extends AnyFunSuite {

  private def pileSize(st: GameState): Int =
    st.pile.allCards.size

  private val names2 = Vector("Player1", "Player2")
  private val names3 = Vector("Player1", "Player2", "Player3")
  private val names4 = Vector("Player1", "Player2", "Player3", "Player4")
  
  test("Deal distribuisce tutte le carte round-robin nelle mani e svuota il deck") {
    val rng   = RNG.default()
    val deck  = DeckBuilder.standardShuffled(rng)
    val st0   = GameState.initial(players = names4.size, names4, deck)

    val (st1, events) = Engine.step(st0, GameCommand.Deal).fold(err => fail(err), identity)

    assert(st1.deck.isEmpty, "Dopo Deal il deck deve essere vuoto")
    assert(st1.hands.size == 4, "Devono esserci 4 mani")
    assert(st1.hands.values.forall(_.size > 0), "Ogni mano deve avere almeno una carta")

    val totalInHands = st1.hands.values.map(_.size).sum
    assert(totalInHands == deck.size, s"Attese ${deck.size} carte distribuite, trovate $totalInHands")

    assert(events.nonEmpty)
  }

  test("Play rimuove le carte dalla mano, le mette nella pila e memorizza la dichiarazione") {
    val rng   = RNG.default()
    val deck  = DeckBuilder.standardShuffled(rng)
    val st0   = GameState.initial(players = names3.size,names3,  deck)
    val (st1, _) = Engine.step(st0, GameCommand.Deal).fold(err => fail(err), identity)

    val current     = st1.turn
    val cardToPlay  = st1.hands(current).cards.head
    val beforeHand  = st1.hands(current).size
    val beforePile  = pileSize(st1)

    val (st2, evs) =
      Engine.step(st1, GameCommand.Play(current, List(cardToPlay), Rank.Ace)).fold(err => fail(err), identity)

    assert(st2.hands(current).size == beforeHand - 1, "La carta giocata deve essere rimossa dalla mano")
    assert(pileSize(st2) == beforePile + 1, "La pila deve crescere di una carta")
    assert(st2.lastDeclaration.nonEmpty, "La dichiarazione deve essere salvata")
    assert(evs.exists {
      case Engine.GameEvent.Played(p, r, cnt) => p == current && r == Rank.Ace && cnt == 1
      case _ => false
    })
  }

  test("CallBluff: se la dichiarazione è falsa il dichiarante prende la pila, altrimenti l'accusatore") {
    val rng   = RNG.default()
    val deck  = DeckBuilder.standardShuffled(rng)
    val st0   = GameState.initial(players = names2.size, names2,  deck)
    val (st1, _) = Engine.step(st0, GameCommand.Deal).fold(err => fail(err), identity)

    val p0 = st1.turn
    val p1 = st1.players.find(_ != p0).get

    // Bluff certo: dichiara Ace ma gioca carta non-Ace
    val notAce = st1.hands(p0).cards.find(_.rank != Rank.Ace).getOrElse(st1.hands(p0).cards.head)
    val (st2, _) = Engine.step(st1, GameCommand.Play(p0, List(notAce), Rank.Ace)).fold(err => fail(err), identity)

    val pileBefore = pileSize(st2)
    val (st3, evs) = Engine.step(st2, GameCommand.CallBluff(p1)).fold(err => fail(err), identity)

    assert(pileSize(st3) == 0, "Dopo la chiamata la pila deve svuotarsi")
    val truthful = false
    val expectedPicker = if truthful then p1 else p0
    val pickedSize = st3.hands(expectedPicker).size
    assert(pickedSize >= st1.hands(expectedPicker).size + pileBefore - 1, "Il ricevente deve aver preso la pila (stima)")

    assert(evs.exists {
      case Engine.GameEvent.BluffCalled(by, _, t) => by == p1 && t == truthful
      case _ => false
    })
  }
}
