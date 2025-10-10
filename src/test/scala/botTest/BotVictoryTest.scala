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

    BotManager.onEvents = _ => ()
    BotManager.onStateUpdate = s => currentState = s
    BotManager.executeCommand = (cmd: GameCommand) =>
      Engine.step(currentState, cmd) match
        case Right((newSt, evs)) =>
          currentState = newSt
          Right((newSt, evs))
        case Left(err) => Left(err)

    BotManager.takeTurn(bot, currentState) match
      case Right((newSt, evs)) =>
        (newSt, evs)
      case Left(err) =>
        fail(s"Bot execution failed: $err")
  }

  test("RandomBot wins the game") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)

    val bot: Bot = BotFactory("facile", p1)

    // Bot ha una carta che può giocare per vincere
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
      turn = p1, // È il turno del bot
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Player", p1 -> "RandomBot"),
      fixedDeclaredRank = Some(Rank.Due), // Il bot deve giocare "Due"
      clocks = Map(p0 -> 60000L, p1 -> 60000L)
    )

    val (newSt, events) = playBot(bot, st)

    println(s"[DEBUG] RandomBot events: $events")
    println(s"[DEBUG] Bot hand size after play: ${newSt.hands(p1).size}")
    println(s"[DEBUG] Game finished: ${events.exists(_.isInstanceOf[GameEvent.GameEnded])}")

    // Verifica che il bot abbia giocato la sua carta
    assert(newSt.hands(p1).size == 0, "RandomBot should have no cards left")

    // Verifica che ci sia un evento GameEnded
    val gameEndedEvents = events.collect { case ge: GameEvent.GameEnded => ge }
    assert(gameEndedEvents.nonEmpty, "Should have a GameEnded event")
    assert(gameEndedEvents.head.winner == p1, "RandomBot should be the winner")
  }

  test("SmartBot wins the game") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)

    val bot: Bot = BotFactory("difficile", p1)

    // Bot ha una carta che corrisponde al rango fisso per vincere facilmente
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
      turn = p1, // È il turno del bot
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Alice", p1 -> "SmartBot"),
      fixedDeclaredRank = Some(Rank.Due), // Il bot ha la carta giusta
      clocks = Map(p0 -> 60000L, p1 -> 60000L)
    )

    val (newSt, events) = playBot(bot, st)

    println(s"[DEBUG] SmartBot events: $events")
    println(s"[DEBUG] Bot hand size after play: ${newSt.hands(p1).size}")
    println(s"[DEBUG] Game finished: ${events.exists(_.isInstanceOf[GameEvent.GameEnded])}")

    // Verifica che il bot abbia giocato la sua carta
    assert(newSt.hands(p1).size == 0, "SmartBot should have no cards left")

    // Verifica che ci sia un evento GameEnded
    val gameEndedEvents = events.collect { case ge: GameEvent.GameEnded => ge }
    assert(gameEndedEvents.nonEmpty, "Should have a GameEnded event")
    assert(gameEndedEvents.head.winner == p1, "SmartBot should be the winner")
  }


  test("Bot handles bluff calling scenario") {
    val p0 = PlayerId(0)
    val p1 = PlayerId(1)

    val bot: Bot = BotFactory("difficile", p1)

    val botCard = Card(Rank.Tre, Suit.Hearts)
    val playerCard = Card(Rank.Quattro, Suit.Spades)

    // Creiamo una situazione con una dichiarazione sospetta nella pila
    val suspiciousCards = List(Card(Rank.Asso, Suit.Clubs), Card(Rank.Due, Suit.Diamonds))
    val declaration = Declaration(p0, Rank.Asso, suspiciousCards)
    val pile = CenterPile.empty.push(suspiciousCards)

    val hands = Map(
      p0 -> Hand(List(playerCard)),
      p1 -> Hand(List(botCard))
    )

    val st = GameState(
      players = Vector(p0, p1),
      hands = hands,
      deck = Nil,
      pile = pile,
      turn = p1,
      lastDeclaration = Some(declaration),
      pendingPenalty = None,
      finished = false,
      playersNames = Map(p0 -> "Player", p1 -> "SmartBot"),
      fixedDeclaredRank = Some(Rank.Asso),
      clocks = Map(p0 -> 60000L, p1 -> 60000L)
    )

    val (newSt, events) = playBot(bot, st)

    println(s"[DEBUG] Bluff scenario events: $events")

    // Il bot potrebbe chiamare bluff o giocare, entrambe sono mosse valide
    val hasValidMove = events.exists {
      case _: GameEvent.BluffCalled => true
      case _: GameEvent.Played => true
      case _: GameEvent.BotPlayed => true
      case _ => false
    }

    assert(hasValidMove, "Bot should make a valid move (either call bluff or play)")
  }
}