package it.unibo.bluff.view.cli

import scala.io.StdIn
import it.unibo.bluff.model.*
import it.unibo.bluff.model.util.RNG
import it.unibo.bluff.model.state.GameState

object CLI:

  private var gameState: Option[GameState] = None
  private var running: Boolean = false

  def start(players: Int = 2): Unit =
    val rng  = RNG.default()
    val deck = DeckBuilder.standardShuffled(rng) // in it.unibo.bluff.model.Rules.scala
    val st   = GameState.initial(players, deck)

    gameState = Some(st)

    println(s"Nuova partita con $players giocatori.")
    println(s"Mazzo iniziale: ${deck.size} carte.")
    println(s"Primo turno al giocatore: ${turnString(st)}")
    println("Comandi: new | status | end-turn | help | quit")

  private def turnString(st: GameState): String =
    // se st.turn è un indice/PlayerId, va bene così; se è una struttura, adatta qui
    st.turn.toString

  private def endTurn(): Unit =
    gameState match
      case Some(st) =>
        val next    = st.nextPlayer
        val updated = st.copy(turn = next)
        gameState = Some(updated)
        println(s"Cambio turno: ora tocca al giocatore $next")
      case None =>
        println("Nessuna partita in corso")

  def statusMessage: String =
    gameState match
      case Some(st) => s"Turno attuale: ${turnString(st)}"
      case None     => "Nessuna partita in corso"

  def execute(input: String): Unit =
    input.trim.toLowerCase match
      case "new" =>
        start(2)
      case "status" =>
        println(statusMessage)
      case "end-turn" =>
        endTurn()
      case "help" | "h" =>
        println("Comandi: new | status | end-turn | help | quit")
      case "quit" | "exit" =>
        running = false
      case other if other.nonEmpty =>
        println(s"Comando sconosciuto: $other")
      case _ => ()

  def repl(): Unit =
    running = true
    while running do
      print("> ")
      val line = Option(StdIn.readLine()).getOrElse("")
      execute(line)