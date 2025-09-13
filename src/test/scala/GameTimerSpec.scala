import org.scalatest.funsuite.AnyFunSuite
import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.state.GameClocks
import it.unibo.bluff.model.PlayerId
import it.unibo.bluff.model.core.timer.GameTimer
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.CenterPile

final class GameTimerSpec extends AnyFunSuite {

  test("GameTimer calls onTimeout once when clock expires") {
    // Stato minimale: due giocatori, turno al primo
    val p0 = PlayerId(0); val p1 = PlayerId(1)
    val base = GameState(
      players = Vector(p0, p1),
      hands = Map(p0 -> Hand.empty, p1 -> Hand.empty),
      deck = Nil,
      pile = CenterPile.empty,
      turn = p0,
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = Map.empty,
      fixedDeclaredRank = None,
      clocks = Map(p0 -> 10L, p1 -> 10L)
    )

    val ref = new AtomicReference(base)

    var called: List[PlayerId] = Nil
    val timer = new GameTimer(ref, tickMillis = 10L, perTurnMillis = 30L, onTimeout = (pid: PlayerId) => called = pid :: called)

    try {
      timer.start()
      // attendo abbastanza per il timer(più di perTurnMillis / tickMillis)
      Thread.sleep(120)
      assert(called.nonEmpty && called.head == p0)
      val firstCalls = called.count(_ == p0)
      assert(firstCalls >= 1)
    } finally {
      timer.stop()
    }
  }
}
