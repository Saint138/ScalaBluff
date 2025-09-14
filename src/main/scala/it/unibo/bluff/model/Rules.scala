package it.unibo.bluff.model

import it.unibo.bluff.model.cards.Card
import it.unibo.bluff.model.core.*

/** Strategia ordine turni (permette estensioni future: reverse, skip, ecc.) */
trait TurnOrder:
  def next(players: Vector[PlayerId], current: PlayerId): PlayerId
object TurnOrder:
  given TurnOrder with
    def next(players: Vector[PlayerId], current: PlayerId): PlayerId =
      val idx = players.indexOf(current)
      players((idx + 1) % players.size)

object DeckBuilder:
  def standardShuffled(rng: util.RNG): List[Card] =
    rng.shuffle(Card.fullDeck)

object BluffResolution:
  enum BluffResult:
    case BluffSuccessful(bluffer: PlayerId)
    case BluffFailed(accuser: PlayerId)

  import BluffResult.*

  /** Verifica se le carte corrispondono al rango dichiarato */
  def check(decl: state.Declaration): Boolean = decl.hiddenCards.forall(_.rank == decl.declared)
