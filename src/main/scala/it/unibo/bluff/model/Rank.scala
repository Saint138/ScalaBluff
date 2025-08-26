package it.unibo.bluff.model

enum Rank {
  case Asso, Due, Tre, Quattro, Cinque, Sei, Sette, Otto, Nove, Dieci, Jack, Queen, King
}

object Rank:
  val order: Vector[Rank] = Vector(Asso, Due, Tre, Quattro, Cinque, Sei, Sette, Otto, Nove, Dieci, Jack, Queen, King)
  given Ordering[Rank] = Ordering.by(order.indexOf)