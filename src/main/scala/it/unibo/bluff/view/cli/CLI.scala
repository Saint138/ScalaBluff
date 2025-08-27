package it.unibo.bluff.view.cli

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.util.RNG

import scala.io.StdIn

object CLI:
  private var gameState: Option[GameState] = None
  def state: Option[GameState] = gameState

  private var running = false

  /** Avvia il REPL principale (menu iniziale) */
  def repl(): Unit =
    running = true
    println("Comandi: new | help | quit")
    while running do
      print("> ")
      val line = Option(StdIn.readLine()).getOrElse("")
      CommandHandler.execute(line.trim, this)

  /** Chiude il REPL */
  def quit(): Unit = running = false

  /** Espone lo stato corrente (per CommandHandler) */
  def currentState: Option[GameState] = gameState

  /** Avvia una nuova partita con input su numero giocatori e nomi */
  def startNewGame(): Unit =
    val numPlayers = promptPlayersCount()
    val names      = promptPlayersName(numPlayers)
    initGame(numPlayers, names)

  /** Esegue un passo dell'engine e gestisce eventi/stato/fine partita */
  def step(state: GameState, cmd: GameCommand): Unit =
    Engine.step(state, cmd) match
      case Left(err) =>
        println(s"Errore: $err")
      case Right((st2, events)) =>
        gameState = Some(st2)
        // Stampa eventi e stato aggiornato (passo st2 per i nomi)
        CLIPrinter.printEvents(events, st2)
        CLIPrinter.printStatus(st2)

        // Chiudi REPL se la partita è finita
        //if events.exists(_.isInstanceOf[GameEvent.GameEnded]) then
          //println("Partita terminata. Digita 'new' per iniziare una nuova partita o 'quit' per uscire.")
          // (opzionale) azzera lo stato per bloccare comandi di gioco post-fine:
          // gameState = None
          //running = false

  // -------------------- Helpers interni --------------------

  private def promptPlayersCount(): Int =
    var count = 0
    while count < 2 || count > 4 do
      print("Inserisci il numero di giocatori (2-4): ")
      count = StdIn.readLine().toIntOption.getOrElse(0)
      if count < 2 || count > 4 then println("Numero non valido.")
    count

  private def promptPlayersName(players: Int): Vector[String] =
    (0 until players).map { i =>
      print(s"Inserisci il nome per il giocatore ${i + 1}: ")
      StdIn.readLine().trim
    }.toVector

  /** Inizializza lo stato e distribuisce subito le carte. */
  private def initGame(numPlayers: Int, names: Vector[String]): Unit =
    val shuffler = Shuffler.random
    val deckObj = Dealing.initialDeckForPlayers(numPlayers, shuffler)
    val deck = deckObj match
      case ListDeck(cs) => cs
    val st   = GameState.initial(players = numPlayers, playerNames = names, shuffled = deck)
    gameState = Some(st)
    println(s"Nuova partita con $numPlayers giocatori.")
    println(s"Mazzo iniziale: ${deck.size} carte.")
    println(s"Primo turno: ${st.nameOf(st.turn)}")
    // Distribuzione iniziale
    step(st, GameCommand.Deal)
