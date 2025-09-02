package it.unibo.bluff.model.state

import it.unibo.bluff.model.*

/** Dichiarazione effettuata: rango dichiarato e carte coperte giocate */
final case class Declaration(player: PlayerId, declared: Rank, hiddenCards: List[Card])

/** Pila centrale delle carte giocate */
final case class CenterPile(stacks: List[List[Card]]):
  def allCards: List[Card] = stacks.flatten
  def push(cs: List[Card]): CenterPile = CenterPile(cs :: stacks)
  def clear: (List[Card], CenterPile) = (allCards, CenterPile(Nil))
object CenterPile:
  val empty = CenterPile(Nil)

/** Stato principale del gioco */
final case class GameState(
  players: Vector[PlayerId],
  hands: Map[PlayerId, Hand],
  deck: List[Card],
  pile: CenterPile,
  turn: PlayerId,
  lastDeclaration: Option[Declaration],
  pendingPenalty: Option[PlayerId],
  finished: Boolean,
  playersNames: Map[PlayerId, String],
  fixedDeclaredRank: Option[Rank],
  /** Remaining time per player in milliseconds. Managed externally by a timer and used by game logic when timeout occurs. */
  clocks: Map[PlayerId, Long]
):
  def nameOf(player: PlayerId): String = playersNames(player)
  def nextPlayer(using order: TurnOrder): PlayerId = order.next(players, turn)
  //card distribution 
  def applyInitialDistribution: GameState =
    if hands.nonEmpty || deck.isEmpty then this
    else
      val (distributed, leftoverDeck) =
        Dealing.dealAll(players.toList, ListDeck(deck))
      val remainingCards = leftoverDeck match
        case ListDeck(cs) => cs
      this.copy(hands = distributed, deck = remainingCards)

  // Clock operations moved to GameClocks helper to keep GameState focused on data

object GameState:
  def initial(players: Int,playerNames: Vector[String], shuffled: List[Card]): GameState =
    val ids = Vector.tabulate(players)(PlayerId.apply)
    val nameMap = ids.zip(playerNames).toMap
    GameState(
      players = ids,
      hands = Map.empty,
      deck = shuffled,
      pile = CenterPile.empty,
      turn = ids.head,
      lastDeclaration = None,
      pendingPenalty = None,
      finished = false,
      playersNames = nameMap,
      fixedDeclaredRank = None,
      clocks = ids.map(_ -> 0L).toMap
    )
