package it.unibo.bluff.engine

import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*
import it.unibo.bluff.model.TurnOrder
//prima implementazione--

// === Game Commands (input dal giocatore / UI) ===
sealed trait GameCommand
object GameCommand:
  /** Distribuzione carte iniziale */
  case object Deal extends GameCommand
  /** Giocare una o più carte dichiarando un rango */
  final case class Play(player: PlayerId, cards: List[Card], declared: Rank) extends GameCommand
  /** Accusare l'ultima dichiarazione */
  final case class CallBluff(player: PlayerId) extends GameCommand

// === Game Events (output del motore) ===
sealed trait GameEvent
object GameEvent:
  case object CardsDealt extends GameEvent
  final case class DeclarationMade(decl: Declaration) extends GameEvent
  final case class BluffCalled(accuser: PlayerId, decl: Declaration, truthful: Boolean) extends GameEvent
  final case class GameEnded(winner: PlayerId) extends GameEvent

object GameEngine:
  /** Punto di ingresso: applica un comando allo stato e produce nuovo stato + eventi */
  def applyCommand(state: GameState, cmd: GameCommand)(using TurnOrder): Either[String,(GameState,List[GameEvent])] =
    cmd match
      case GameCommand.Deal => deal(state)
      case p: GameCommand.Play => play(state, p)
      case b: GameCommand.CallBluff => callBluff(state, b)
      
  private def deal(state: GameState): Either[String,(GameState,List[GameEvent])] =
    if state.hands.nonEmpty then Left("Carte già distribuite")
    else
      val players = state.players
      val dealt: Map[PlayerId, List[Card]] =
        state.deck.zipWithIndex.groupBy(_._2 % players.size).view.mapValues(_.map(_._1)).toMap
          .map{ case (idx, cs) => players(idx) -> cs } 
      val hands = dealt.view.mapValues(cs => Hand(cs)).toMap
      val newState = state.copy(hands = hands, deck = Nil)
      Right(newState -> List(GameEvent.CardsDealt))

  private def play(state: GameState, cmd: GameCommand.Play)(using TurnOrder): Either[String,(GameState,List[GameEvent])] =
    for
      _ <- Either.cond(cmd.player == state.turn, (), "Non è il turno del giocatore")
      hand <- state.hands.get(cmd.player).toRight("Mano inesistente")
      updatedHand <- hand.remove(cmd.cards)
      _ <- Either.cond(cmd.cards.nonEmpty, (), "Nessuna carta giocata")
      decl = Declaration(cmd.player, cmd.declared, cmd.cards)
      pile2 = state.pile.push(cmd.cards)
      st2 = state.copy(
        hands = state.hands.updated(cmd.player, updatedHand),
        pile = pile2,
        lastDeclaration = Some(decl),
        turn = state.nextPlayer
      )
    yield st2 -> List(GameEvent.DeclarationMade(decl))

  private def callBluff(state: GameState, cmd: GameCommand.CallBluff): Either[String,(GameState,List[GameEvent])] =
    state.lastDeclaration.toRight("Nessuna dichiarazione da accusare").map{ decl =>
      val truthful = decl.hiddenCards.forall(_.rank == decl.declared)
      val pileCards = state.pile.allCards
      // Decide chi prende il mazzo
      val (receiver, nextTurn) =
        if truthful then (cmd.player, decl.player)      // accusa fallita
        else (decl.player, cmd.player)                  // bluff riuscito
      val receiverHand = state.hands.getOrElse(receiver, Hand(Nil)).addAll(pileCards)
      val newHands = state.hands.updated(receiver, receiverHand)
      val clearedPile = state.pile.clear._2
      val st2 = state.copy(
        hands = newHands,
        pile = clearedPile,
        lastDeclaration = None,
        turn = nextTurn
      )
      st2 -> List(GameEvent.BluffCalled(cmd.player, decl, truthful))
    }
