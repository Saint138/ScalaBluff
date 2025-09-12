package it.unibo.bluff.model.core.engine

import it.unibo.bluff.model.*
import it.unibo.bluff.model.TurnOrder
import it.unibo.bluff.model.core.state.{Declaration, GameState}

object Engine:

  sealed trait GameCommand
  object GameCommand:
    case object Deal extends GameCommand
    final case class Play(player: PlayerId, cards: List[Card], declared: Rank) extends GameCommand
    final case class CallBluff(player: PlayerId) extends GameCommand
    final case class Timeout(player: PlayerId) extends GameCommand

  sealed trait GameEvent
  object GameEvent:
    final case class Dealt(handsSize: Map[PlayerId, Int]) extends GameEvent
    final case class Played(player: PlayerId, declared: Rank, count: Int) extends GameEvent
    final case class BotPlayed(player: PlayerId, declared: Rank, count: Int) extends GameEvent
    final case class BluffCalled(by: PlayerId, against: Declaration, truthful: Boolean) extends GameEvent
    final case class TimerExpired(player: PlayerId) extends GameEvent
    final case class QuartetCleared(player: PlayerId, rank: Rank, count: Int) extends GameEvent
    final case class GameEnded(winner: PlayerId) extends GameEvent

  import GameCommand.*
  import GameEvent.*

  /** Single state transition */
  def step(state: GameState, cmd: GameCommand)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
    for
      _ <- ensureNotEnded(state) 
      res <- cmd match
        case Deal          => Right(deal(state))
        case p: Play       => play(state, p)
        case c: CallBluff  => call(state, c)
        case t: Timeout    => timeout(state, t)
      (st2, evs) = res
      (st3, autoEvs) = sweepQuartets(st2)
      out = withWinEvent(st3, evs ++ autoEvs) 
    yield out

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
      _        <- if cmd.cards.nonEmpty then Right(()) else Left("Devi giocare almeno una carta")
      _        <- state.fixedDeclaredRank match
                    case Some(r) if r != cmd.declared => Left(s"Devi dichiarare $r")
                    case _ => Right(())
      newHand  <- ensureOwns(state, cmd.player, cmd.cards)
    yield
      val updatedHands = state.hands.updated(cmd.player, newHand)
      val newPile = state.pile.push(cmd.cards)
      val decl = Declaration(cmd.player, cmd.declared, cmd.cards)
      val next = state.nextPlayer
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

  private def timeout(state: GameState, cmd: Timeout)(using TurnOrder): Either[String, (GameState, List[GameEvent])] =
    val p = cmd.player
    if state.turn != p then Left(s"Timeout di ${p.value} ma non è il suo turno")
    else
      val remaining = state.clocks.getOrElse(p, 0L)
      if remaining > 0L then Left(s"Il timer non è ancora scaduto per ${p.value}: $remaining ms rimanenti")
      else
        val pileCards = state.pile.allCards
        val receiverHand = state.hands.getOrElse(p, Hand(Nil)).addAll(pileCards)
        val newHands = state.hands.updated(p, receiverHand)
        val (_, cleared) = state.pile.clear
        val next = state.nextPlayer
        val st2 = state.copy(
          hands = newHands,
          pile = cleared,
          lastDeclaration = None,
          turn = next,
          fixedDeclaredRank = None
        )
        Right(st2 -> List(GameEvent.TimerExpired(p)))


  private def sweepQuartets(state: GameState): (GameState, List[GameEvent]) =
    var changed = false
    var allEvents = List.empty[GameEvent]

    val newHands: Map[PlayerId, Hand] = state.hands.map { case (pid, hand) =>
      val counts = hand.cards.groupBy(_.rank).view.mapValues(_.size).toMap
      val toRemove: Map[Rank, Int] = counts.collect { case (r, c) if c >= 4 => r -> (c / 4 * 4) }.toMap
      if toRemove.isEmpty then pid -> hand
      else
        changed = true
        val remainingToRemove = scala.collection.mutable.Map.from(toRemove)
        val kept = scala.collection.mutable.ListBuffer.empty[Card]
        hand.cards.foreach { c =>
          val r = c.rank
          val n = remainingToRemove.getOrElse(r, 0)
          if n > 0 then remainingToRemove.update(r, n - 1)
          else kept += c
        }
       
        allEvents ++= toRemove.iterator.map { case (r, k) => GameEvent.QuartetCleared(pid, r, k) }
        pid -> Hand(kept.toList)
    }

    if changed then (state.copy(hands = newHands), allEvents) else (state, Nil)

  private def ensureNotEnded(state: GameState): Either[String, Unit] =
    winnerIfAny(state)
      .map(pid => s"Partita terminata: ha già vinto ${state.nameOf(pid)}")
      .toLeft(())

  private def winnerIfAny(state: GameState): Option[PlayerId] =
    state.hands.collectFirst { case (pid, hand) if hand.size == 0 => pid }

  private def withWinEvent(st: GameState, evs: List[GameEvent]): (GameState, List[GameEvent]) =
    winnerIfAny(st) match
      case Some(w) => st -> (evs :+ GameEnded(w))
      case None    => st -> evs


object GameEngine:
  export Engine.GameEvent
  export Engine.GameCommand
  def applyCommand(state: GameState, cmd: Engine.GameCommand)(using TurnOrder): Either[String, (GameState, List[Engine.GameEvent])] =
    Engine.step(state, cmd)
