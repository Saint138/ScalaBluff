package botTest

import it.unibo.bluff.model.*
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.cards.{Card, Rank, Suit}
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.*
import it.unibo.bluff.model.core.engine.Engine.GameEvent.*
import it.unibo.bluff.model.core.state.*
import it.unibo.bluff.model.bot.{BotFactory, Bot}
import org.scalatest.funsuite.AnyFunSuite

class BotBluffTurnTest extends AnyFunSuite {

  def playBotTurn(bot: Bot, st: GameState): (GameState, List[GameEvent]) = {
    val cmd = bot.decideMove(st)
    Engine.step(st, cmd).toOption.get
  }

  test("Player vs RandomBot: bluff vero") {
    val p0 = PlayerId(0) // giocatore umano
    val p1 = PlayerId(1) // bot

    given TurnOrder = TurnOrder.given_TurnOrder

    val bot: Bot = BotFactory("facile", p1)

    val hands = Map(
      p0 -> Hand(List(Card(Rank.Tre, Suit.Hearts), Card(Rank.Due, Suit.Spades))),
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

    val (newSt, events) = playBotTurn(bot, st)
    println(s"[DEBUG] Bluff vero vs RandomBot: turno finale=${newSt.turn}")
    println(s"[DEBUG] Eventi generati: $events")

    assert(events.exists {
      case BluffCalled(by, _, truthful) => by == p1
      case _ => false
    } || events.exists {
      case BotPlayed(_, _, _) => true
      case _ => false
    })
  }

  test("Player vs SmartBot: bluff falso") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)

    given TurnOrder = TurnOrder.given_TurnOrder

    val bot: Bot = BotFactory("difficile", p1)

    val hands = Map(
      p0 -> Hand(List(Card(Rank.Asso, Suit.Hearts), Card(Rank.Due, Suit.Spades))),
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
      playersNames = Map(p0 -> "Alice", p1 -> "SmartBot"),
      fixedDeclaredRank = Some(Rank.Due),
      clocks = Map(p0 -> 0L, p1 -> 0L)
    )

    // Gioca il turno del bot
    val (newSt, events) = playBotTurn(bot, st)
    println(s"[DEBUG] Bluff falso vs SmartBot: turno finale=${newSt.turn}")
    println(s"[DEBUG] Eventi generati: $events")

    // Assert corretto: il bot ha giocato una carta (Played) o ha chiamato bluff
    assert(events.exists {
      case Played(by, _, _) => by == p1 // controlla che il bot abbia giocato
      case BluffCalled(by, _, _) => by == p1 // controlla eventuale chiamata bluff
      case _ => false
    }, s"Nessun evento valido generato dal bot: $events")
  }
}