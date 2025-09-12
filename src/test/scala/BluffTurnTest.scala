import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.engine.Engine.*
import it.unibo.bluff.model.core.state.*
import it.unibo.bluff.model.TurnOrder
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.GameEvent._

class BluffTurnTest extends AnyFunSuite {

  test("Bluff vero: turno passa al dichiarante") {
    val p0 = PlayerId(0) // giocatore
    val p1 = PlayerId(1) // bot
    given TurnOrder = TurnOrder.given_TurnOrder

    // Stato iniziale con dichiarazione p0 vera
    val hands = Map(
      p0 -> Hand(List(Card(Rank.Tre, Suit.Hearts), Card(Rank.Due, Suit.Spades))), // almeno 2 carte
      p1 -> Hand(List(Card(Rank.Asso, Suit.Spades), Card(Rank.Tre, Suit.Diamonds)))
    )

    val decl = Declaration(p0, Rank.Tre, List(Card(Rank.Tre, Suit.Hearts))) // dichiarazione vera
    val st = GameState(
      players = Vector(p0, p1),
      hands = hands,
      deck = Nil,
      pile = CenterPile.empty.push(decl.hiddenCards),
      turn = p1,
      lastDeclaration = Some(decl),
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Alice", p1 -> "Bot"),
      fixedDeclaredRank = Some(Rank.Tre),
      clocks = Map(p0 -> 0L, p1 -> 0L)
    )

    val result = Engine.step(st, GameCommand.CallBluff(p1))
    assert(result.isRight, s"Engine.step ha restituito Left: $result")

    val (newSt, events) = result.toOption.get
    println(s"[DEBUG Test] Bluff vero: turno iniziale p1=${st.turn}, turno finale=${newSt.turn}")
    println(s"[DEBUG Test] Eventi generati: $events")

    assert(newSt.turn == p0, "Bluff vero → il turno dovrebbe passare al dichiarante")
    assert(events.exists {
      case BluffCalled(by, _, truthful) => by == p1 && truthful
      case _ => false
    })
  }

  test("Bluff falso: turno passa a chi chiama") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)
    given TurnOrder = TurnOrder.given_TurnOrder

    // Stato iniziale con bluff
    val hands = Map(
      p0 -> Hand(List(Card(Rank.Asso, Suit.Hearts), Card(Rank.Due, Suit.Spades))), // dichiarante ha almeno 2 carte
      p1 -> Hand(List(Card(Rank.Tre, Suit.Diamonds), Card(Rank.Quattro, Suit.Clubs)))
    )

    val bluffDecl = Declaration(p0, Rank.Due, List(Card(Rank.Asso, Suit.Hearts))) // bluff
    val st = GameState(
      players = Vector(p0, p1),
      hands = hands,
      deck = Nil,
      pile = CenterPile.empty.push(bluffDecl.hiddenCards),
      turn = p1,
      lastDeclaration = Some(bluffDecl),
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Alice", p1 -> "Bot"),
      fixedDeclaredRank = Some(Rank.Due),
      clocks = Map(p0 -> 0L, p1 -> 0L)
    )

    val result = Engine.step(st, GameCommand.CallBluff(p1))
    assert(result.isRight, s"Engine.step ha restituito Left: $result")

    val (newSt, events) = result.toOption.get
    println(s"[DEBUG Test] Bluff falso: turno iniziale p1=${st.turn}, turno finale=${newSt.turn}")
    println(s"[DEBUG Test] Eventi generati: $events")

    assert(newSt.turn == p1, "Bluff falso → il turno dovrebbe passare a chi chiama")
    assert(events.exists {
      case BluffCalled(by, _, truthful) => by == p1 && !truthful
      case _ => false
    })
  }
}