import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.model.*
import it.unibo.bluff.engine.*
import it.unibo.bluff.model.state.GameState

class GameScenarioTest extends AnyFunSuite:
  // Turn order locale (semplice ordine circolare)
  given TurnOrder with
    def next(players: Vector[PlayerId], current: PlayerId): PlayerId =
      val idx = players.indexOf(current)
      players((idx + 1) % players.size)

  test("bluff chiamato correttamente") {
    // Setup mazzo deterministico (prime 6 carte only)
    val p0 = PlayerId(0); val p1 = PlayerId(1)
    val cards = List(
      Card(Rank.Ace, Suit.Clubs), Card(Rank.Two, Suit.Clubs), Card(Rank.Three, Suit.Clubs),
      Card(Rank.Four, Suit.Clubs), Card(Rank.Five, Suit.Clubs), Card(Rank.Six, Suit.Clubs)
    )
    val state0 = GameState.initial(2, cards)
    val (st1, _) = GameEngine.applyCommand(state0, GameCommand.Deal).fold(err => fail(err), identity)
    // Giocatore 0 gioca 1 carta dichiarando Ace (vera)
    val hand0First = st1.hands(p0).cards.head
    val (st2, _) = GameEngine.applyCommand(st1, GameCommand.Play(p0, List(hand0First), Rank.Ace)).fold(err => fail(err), identity)
    // Giocatore 1 chiama bluff (che fallisce perché dichiarazione vera): penalizzato p1
    val (st3, events) = GameEngine.applyCommand(st2, GameCommand.CallBluff(p1)).fold(err => fail(err), identity)
    assert(st3.hands(p1).cards.size > st2.hands(p1).cards.size)
    assert(events.exists{ case GameEvent.BluffCalled(_, _, truthful) => truthful; case _ => false })
  }
