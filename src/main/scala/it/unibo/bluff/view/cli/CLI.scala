package it.unibo.bluff.view.cli

import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.DeckBuilder
import it.unibo.bluff.engine.Engine


import scala.io.StdIn
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.util.RNG

object CLI:
  private var gameState: Option[GameState] = None
  private var running = false

  def repl(): Unit =
    running = true
    println("Comandi: new | help | quit")
    while running do
      print("> ")
      val line = Option(StdIn.readLine()).getOrElse("")
      CommandHandler.execute(line.trim, this)

  def quit(): Unit = running = false

  def currentState: Option[GameState] = gameState

  def startNewGame(): Unit =
    val numPlayers = promptPlayersCount()
    val names = promptPlayersName(numPlayers)
    initGame(numPlayers, names)

  def step(state: GameState, cmd: GameCommand): Unit =
    Engine.step(state, cmd) match
      case Left(err) => println(s"Errore: $err")
      case Right((st2, events)) =>
        gameState = Some(st2)
        CLIPrinter.printEvents(events)
        CLIPrinter.printStatus(st2)

  private def promptPlayersCount(): Int =
    var count = 0
    while count < 2 || count > 4 do
      print("Inserisci il numero di giocatori (2-4): ")
      count = StdIn.readLine().toIntOption.getOrElse(0)
      if count < 2 || count > 4 then println("Numero non valido.")
    count

  private def promptPlayersName(players: Int): Vector[String] =
    (0 until players).map { i=>
      print(s"Inserisci il nome per il giocatore ${i + 1}: ")
      StdIn.readLine().trim
    }.toVector

  private def initGame(numPlayers: Int, names: Vector[String]) : Unit =
    val rng = RNG.default()
    val deck = DeckBuilder.standardShuffled(rng)
    val st = GameState.initial(players = numPlayers, playerNames = names, shuffled = deck)
    gameState = Some(st)
    println(s"Nuova partita con $numPlayers giocatori.")
    println(s"Mazzo iniziale: ${deck.size} carte.")
    println(s"Primo turno: ${st.nameOf(st.turn)}")
    step(st, GameCommand.Deal)
