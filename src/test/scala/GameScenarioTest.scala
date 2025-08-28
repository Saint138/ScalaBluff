import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.engine.GameEngine   // shim che delega a Engine.step
import it.unibo.bluff.engine.GameEngine.GameCommand
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*
import it.unibo.bluff.model.util.RNG
import it.unibo.bluff.model.Rank

// TurnOrder implicito
import it.unibo.bluff.model.TurnOrder.given

final class GameScenarioTest extends AnyFunSuite {

  private def pileSize(st: GameState): Int =
    st.pile.allCards.size

  private val names2 = Vector("Player1", "Player2")
  
  test("Scenario: Deal → Play (vero) → CallBluff (vero → accusa fallita, accusatore prende la pila)") {
    val rng    = RNG.default()
    val deck   = DeckBuilder.standardShuffled(rng)
    val state0 = GameState.initial(players = names2.size,  names2 ,  deck)

    val (st1, _) = GameEngine.applyCommand(state0, GameCommand.Deal).fold(err => fail(err), identity)

    val p0 = st1.turn
    val p1 = st1.players.find(_ != p0).get

    // Gioco carta vera dichiarando il suo rank
    val c = st1.hands(p0).cards.head
    val (st2, _) = GameEngine.applyCommand(st1, GameCommand.Play(p0, List(c), c.rank)).fold(err => fail(err), identity)

    val (st3, events) = GameEngine.applyCommand(st2, GameCommand.CallBluff(p1)).fold(err => fail(err), identity)

    assert(pileSize(st3) == 0)
    assert(events.exists {
      case GameEngine.GameEvent.BluffCalled(by, _, truthful) => by == p1 && truthful
      case _ => false
    })
  }

  test("Scenario: Deal → Play (falso) → CallBluff (vero → pila al dichiarante)") {
    val rng   = RNG.default()
    val deck  = DeckBuilder.standardShuffled(rng)
    val st0   = GameState.initial(players = names2.size, names2, deck)

    val (st1, _) = GameEngine.applyCommand(st0, GameCommand.Deal).fold(err => fail(err), identity)

    val p0 = st1.turn
    val p1 = st1.players.find(_ != p0).get

    val nonAce = st1.hands(p0).cards.find(_.rank != Rank.Asso).getOrElse(st1.hands(p0).cards.head)
    val (st2, _) = GameEngine.applyCommand(st1, GameCommand.Play(p0, List(nonAce), Rank.Asso)).fold(err => fail(err), identity)

    val pileBefore = pileSize(st2)
    val (st3, evs) = GameEngine.applyCommand(st2, GameCommand.CallBluff(p1)).fold(err => fail(err), identity)

    assert(pileSize(st3) == 0, "La pila deve svuotarsi dopo CallBluff")
    // in caso di bluff falso: il dichiarante raccoglie la pila
    assert(st3.hands(p0).size >= st1.hands(p0).size + pileBefore - 1)

    assert(evs.exists {
      case GameEngine.GameEvent.BluffCalled(by, _, truthful) => by == p1 && !truthful
      case _ => false
    })
  }
}
