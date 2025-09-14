package it.unibo.bluff.model

import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.model.core.state.*
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.GameCommand.*
import it.unibo.bluff.model.core.engine.Engine.GameEvent
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.cards.{Card, Rank, Suit}

class BotVictoryTest extends AnyFunSuite {

  test("Bot wins the game") {
    val p0 = PlayerId(0) // umano
    val p1 = PlayerId(1) // bot
    given TurnOrder = TurnOrder.given_TurnOrder

    // setup: bot ha 1 carta, il player ha una carta inutile
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
      turn = p1, // facciamo partire subito il bot
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Alice", p1 -> "Bot"),
      fixedDeclaredRank = Some(Rank.Due),
      clocks = Map(p0 -> 0L, p1 -> 0L)
    )

    // il bot gioca la sua unica carta
    val move = Play(p1, List(botCard), Rank.Due)
    val result = Engine.step(st, move)

    assert(result.isRight, s"Engine step failed: $result")
    val (newSt, events) = result.toOption.get

    // dopo la mossa il bot non ha più carte → partita finita
    assert(newSt.finished, "Game should be finished")
    assert(newSt.hands(p1).size.equals(0), "Bot should have no cards left")
    //assert(newSt.winner.contains(p1), "Winner should be the bot")
  }
}
