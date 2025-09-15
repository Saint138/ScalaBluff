package botTest

import it.unibo.bluff.model.{Hand, PlayerId, TurnOrder}
import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.model.core.state.*
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.bot.{Bot, BotFactory, BotManager}
import it.unibo.bluff.model.cards.{Card, Rank, Suit}

class BotVictoryTest extends AnyFunSuite {

  /** Helper per far giocare un bot e ottenere lo stato aggiornato + eventi */
  def playBot(bot: Bot, st: GameState): (GameState, List[GameEvent]) = {
    var currentState = st

    // Creiamo un BotManager locale con executeCommand che aggiorna currentState
    val botManager = new BotManager(
      onEvents = _ => (), // ignoriamo eventi GUI nel test
      onStateUpdate = s => currentState = s, // aggiorniamo currentState
      executeCommand = (cmd: GameCommand) =>
        Engine.step(currentState, cmd)
    )

    botManager.takeTurn(bot, currentState) match
      case Right((newSt, evs)) =>
        currentState = newSt
        (newSt, evs)
      case Left(err) =>
        fail(s"Bot execution failed: $err")
  }

  test("RandomBot wins the game") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)
    given TurnOrder = TurnOrder.given_TurnOrder

    val bot: Bot = BotFactory("random", p1)

    val botCard = Card(Rank.Due, Suit.Hearts)
    val playerCard = Card(Rank.Tre, Suit.Spades)

    val hands = Map(
      p0 -> Hand(List(playerCard)),
      p1 -> Hand(List(botCard))
    )

    val st = GameState(
      players = Vector(p0, p1),
      hands = hands,
      deck = Nil,
      pile = CenterPile.empty,
      turn = p1,
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Player", p1 -> "RandomBot"),
      fixedDeclaredRank = Some(Rank.Due),
      clocks = Map(p0 -> 0L, p1 -> 0L)
    )

    val (newSt, events) = playBot(bot, st)

    println(s"[DEBUG] RandomBot events: $events")
    assert(newSt.hands(p1).size == 0, "RandomBot should have no cards left")
  }

  test("SmartBot wins the game") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)
    given TurnOrder = TurnOrder.given_TurnOrder

    val bot: Bot = BotFactory("smart", p1)

    val botCard = Card(Rank.Due, Suit.Hearts)
    val playerCard = Card(Rank.Tre, Suit.Spades)

    val hands = Map(
      p0 -> Hand(List(playerCard)),
      p1 -> Hand(List(botCard))
    )

    val st = GameState(
      players = Vector(p0, p1),
      hands = hands,
      deck = Nil,
      pile = CenterPile.empty,
      turn = p1,
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Alice", p1 -> "SmartBot"),
      fixedDeclaredRank = Some(Rank.Due),
      clocks = Map(p0 -> 0L, p1 -> 0L)
    )

    val (newSt, events) = playBot(bot, st)

    println(s"[DEBUG] SmartBot events: $events")
    assert(newSt.hands(p1).size == 0, "SmartBot should have no cards left")
  }
}
