package it.unibo.bluff.engine

import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*
import it.unibo.bluff.model.TurnOrder
  /** Distribuzione carte iniziale */

/**
  * Turn engine for Bluff/Dubito aligned to your domain models.
  *
  * - Commands: Deal, Play, CallBluff
  * - Pure reducer API: `Engine.step(state, cmd)(using TurnOrder)`
  * - Round-robin turn order (delegated to given TurnOrder)
  * - Center pile management (uses state.CenterPile)
  * - Accuse last declaration: if truthful → accuser picks the pile, else the declarer picks the pile.
  * - After a bluff resolution, the picker becomes the next player (common rule); customize below if needed.
  * - Game end: when any hand reaches 0 cards → emits GameEnded(winner) and refuses further moves.
  */
object Engine:

  // ===== Commands (player inputs) =====
  sealed trait GameCommand
  object GameCommand:
    /** Distribuzione carte iniziale su tutte le mani */
    case object Deal extends GameCommand
    /** Giocare una o più carte coperte dichiarando un rango */
    final case class Play(player: PlayerId, cards: List[Card], declared: Rank) extends GameCommand
    /** Accusare l'ultima dichiarazione effettuata */
    final case class CallBluff(player: PlayerId) extends GameCommand

  // ===== Events (outputs for UI/logging) =====
  sealed trait GameEvent
  object GameEvent:
    final case class Dealt(handsSize: Map[PlayerId, Int]) extends GameEvent
    final case class Played(player: PlayerId, declared: Rank, count: Int) extends GameEvent
    final case class BluffCalled(by: PlayerId, against: Declaration, truthful: Boolean) extends GameEvent
    // --- NEW: evento di fine partita
    final case class GameEnded(winner: PlayerId) extends GameEvent

  import GameCommand.*
  import GameEvent.*

  /** Single state transition */
  def step(state: GameState, cmd: GameCommand)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
    for
      _ <- ensureNotEnded(state) // NEW: blocca mosse dopo la fine
      res <- cmd match
        case Deal          => Right(deal(state))
        case p: Play       => play(state, p)
        case c: CallBluff  => call(state, c)
      (st2, evs) = res
      out = withWinEvent(st2, evs) // NEW: controlla vittoria dopo la mossa
    yield out

  // ===== Implementation =====

  /**
    * Deal all deck cards to all players in round-robin. Idempotent: if deck is empty it returns Nil events.
    */
  private def deal(state: GameState): (GameState, List[GameEvent]) =
    if state.deck.isEmpty then
      state -> Nil
    else
      val n = state.players.size
      val emptyHands: Map[PlayerId, Hand] = state.players.map(_ -> Hand(Nil)).toMap
      // Round-robin distribution
      val hands = state.deck.zipWithIndex.foldLeft(emptyHands) { case (accHands, (card, i)) =>
        val pid = state.players(i % n)
        accHands.updated(pid, Hand(card :: accHands(pid).cards))
      }
      val newState = state.copy(hands = hands, deck = Nil)
      val sizes = hands.view.mapValues(_.size).toMap
      newState -> List(Dealt(sizes))

  private def ensureTurn(state: GameState, player: PlayerId): Either[String, Unit] =
    if state.turn == player then Right(()) else Left(s"Non è il turno del giocatore ${player.value}. Atteso: ${state.turn.value}")

  private def ensureOwns(state: GameState, player: PlayerId, cards: List[Card]): Either[String, Hand] =
    val hand = state.hands.getOrElse(player, Hand(Nil))
    hand.remove(cards)

  private def play(state: GameState, cmd: Play)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
    for
      _        <- ensureTurn(state, cmd.player)
      // enforce non-empty play
      _        <- if cmd.cards.nonEmpty then Right(()) else Left("Devi giocare almeno una carta")
      // enforce fixed declared rank if present
      _        <- state.fixedDeclaredRank match
                    case Some(r) if r != cmd.declared => Left(s"Devi dichiarare $r")
                    case _ => Right(())
      newHand  <- ensureOwns(state, cmd.player, cmd.cards)
    yield
      val updatedHands = state.hands.updated(cmd.player, newHand)
      val newPile = state.pile.push(cmd.cards)
      val decl = Declaration(cmd.player, cmd.declared, cmd.cards)
      val next = state.nextPlayer
      // If there was no fixedDeclaredRank, set it to this declared rank; otherwise keep existing
      val newFixed = state.fixedDeclaredRank.orElse(Some(cmd.declared))
      val st1 = state.copy(
        hands = updatedHands,
        pile = newPile,
        lastDeclaration = Some(decl),
        turn = next,
        fixedDeclaredRank = newFixed
      )
      st1 -> List(Played(cmd.player, cmd.declared, cmd.cards.size))

  private def call(state: GameState, cmd: CallBluff)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
    state.lastDeclaration match
      case None => Left("Nessuna dichiarazione da accusare")
      case Some(decl) =>
        if decl.player == cmd.player then Left("Non puoi accusare te stesso")
        else
          val truthful = decl.hiddenCards.forall(_.rank == decl.declared)
          val pileCards = state.pile.allCards
          val (receiver, nextTurn) =
            if truthful then (cmd.player, decl.player)   // accusa fallita → accuser prende il mazzo, tocca al dichiarante
            else           (decl.player, cmd.player)     // bluff riuscito  → dichiarante prende il mazzo, tocca all'accusatore

          val receiverHand = state.hands.getOrElse(receiver, Hand(Nil)).addAll(pileCards)
          val newHands = state.hands.updated(receiver, receiverHand)
          val (_, cleared) = state.pile.clear

          val st2 = state.copy(
            hands = newHands,
            pile = cleared,
            lastDeclaration = None,
            turn = nextTurn,
            fixedDeclaredRank = None
          )
          Right(st2 -> List(BluffCalled(cmd.player, decl, truthful)))

  // ===== NEW: helpers per gestione fine partita =====

  /** Rifiuta mosse se qualcuno ha già vinto. */
  private def ensureNotEnded(state: GameState): Either[String, Unit] =
    winnerIfAny(state)
      .map(pid => s"Partita terminata: ha già vinto ${state.nameOf(pid)}") // <-- mappa PlayerId in String
      .toLeft(()) // Option[String] -> Either[String, Unit]

  /** Ritorna il vincitore se trova una mano a 0. */
  private def winnerIfAny(state: GameState): Option[PlayerId] =
    state.hands.collectFirst { case (pid, hand) if hand.size == 0 => pid }

  /** Appende GameEnded se rileva una mano a 0. */
  private def withWinEvent(st: GameState, evs: List[GameEvent]): (GameState, List[GameEvent]) =
    winnerIfAny(st) match
      case Some(w) => st -> (evs :+ GameEnded(w))
      case None    => st -> evs


// ---- Backward-compat shim for old tests ----
object GameEngine:
  export Engine.GameEvent
  export Engine.GameCommand
  def applyCommand(state: GameState, cmd: Engine.GameCommand)(using TurnOrder): Either[String, (GameState, List[Engine.GameEvent])] =
    Engine.step(state, cmd)
