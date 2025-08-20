package it.unibo.bluff.view.cli

import it.unibo.bluff.model.*
import it.unibo.bluff.model.util.RNG
import it.unibo.bluff.model.state.GameState

object CLI:

  var gameState: Option[GameState] = None

  def start(players: Int = 2): Unit =
    val rng = RNG.default()
    val deck = DeckBuilder.standardShuffled(rng) // DeckBuilder è in it.unibo.bluff.model.Rules.scala
    val st = GameState.initial(players, deck)

    gameState = Some(st)

    println(s"Nuova partita con $players giocatori.")
    println(s"Mazzo iniziale: ${deck.size} carte.")
    println(s"Primo turno al giocatore: ${st.turn}")

  private def endTurn(): Unit =
    gameState match
      case Some(st) =>
        val next = st.nextPlayer
        val updated = st.copy(turn = next)
        gameState = Some(updated)
        println(s"Cambio turno: ora tocca al giocatore $next")
      case None =>
        println("Nessuna partita in corso")

  def statusMessage: String =
    gameState match
      case Some(st) => s"Turno attuale: ${st.turn}"
      case None => "Nessuna partita in corso"


  def execute(input: String): Unit =
    input.trim.toLowerCase match
      case "new" =>
        start(2)

      case "status" =>
        println(statusMessage)

      case "end-turn" =>
       endTurn()

      case other =>
        println(s"Comando sconosciuto: $other")


