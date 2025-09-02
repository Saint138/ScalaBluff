package it.unibo.bluff.view.cli

import it.unibo.bluff.controller.GameController
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.bot.RandomBot
import scala.io.StdIn

object CLI:
  private val controller = new GameController()
  private var running = false

  def repl(): Unit =
    running = true
    println("Comandi: new | bot | help | quit")
    while running do
      print("> ")
      val line = Option(StdIn.readLine()).getOrElse("")
      CommandHandler.execute(line.trim, this)
      // se è il turno del bot, facciamo giocare il bot automaticamente
      controller.botTurn() match
        case Right(events) if events.nonEmpty =>
          events.foreach(ev => CLIPrinter.printEvents(List(ev), controller.currentState.get))
          controller.currentState.foreach(CLIPrinter.printStatus)
          controller.currentState.foreach(CLIPrinter.printHand)
        case Left(err) => println(s"Errore bot: $err")
        case _ => ()

  def quit(): Unit =
    running = false
    println("Arrivederci!")

  def currentState: Option[GameState] = controller.currentState

  def startNewGame(): Unit =
    val numPlayers = promptPlayersCount()
    val names = promptPlayersName(numPlayers)
    val deck = Dealing.initialDeckForPlayers(numPlayers, Shuffler.random) match
      case ListDeck(cs) => cs
    controller.newGame(numPlayers, names, deck) match
      case Right(evs) =>
        println("Nuova partita iniziata!")
        controller.currentState.foreach { st =>
          CLIPrinter.printEvents(evs, st)
          CLIPrinter.printStatus(st)
          CLIPrinter.printHand(st)
        }
      case Left(err) => println(s"Errore: $err")

  def startNewGameVSBot(): Unit =
    controller.newGameVSBot() match
      case Right(evs) =>
        println("Nuova partita contro il Bot!")
        controller.currentState.foreach { st =>
          CLIPrinter.printEvents(evs, st)
          CLIPrinter.printStatus(st)
          CLIPrinter.printHand(st)
        }
      case Left(err) => println(s"Errore: $err")


  /** Esegue un passo dell'engine e gestisce eventi/stato/fine partita */
  def step(state: GameState, cmd: GameCommand): Unit =
    controller.handleCommand(cmd) match
      case Left(err) => println(s"Errore: $err")
      case Right(events) =>
        controller.currentState.foreach { st =>
          CLIPrinter.printEvents(events, st)
          CLIPrinter.printStatus(st)
          CLIPrinter.printHand(st)
        }


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

  /** Inizializza lo stato e distribuisce le carte */
  private def initGame(numPlayers: Int, names: Vector[String]): Unit =
    val (stDealt, dealtEvents, deckSize) = fairInitialDeal(numPlayers, names)
    controller.setGameState(stDealt)
    println(s"Nuova partita con $numPlayers giocatori.")
    println(s"Mazzo iniziale: $deckSize carte.")
    println(s"Primo turno: ${stDealt.nameOf(stDealt.turn)}")
    CLIPrinter.printEvents(dealtEvents, stDealt)
    CLIPrinter.printStatus(stDealt)
    CLIPrinter.printHand(stDealt)

  /** Loop di deal: re-shuffle finché nessun giocatore ha 4 carte dello stesso rango */
  private def fairInitialDeal(numPlayers: Int, names: Vector[String]): (GameState, List[GameEvent], Int) =
    val MaxAttempts = 100
    var attempt = 0
    var lastGood: Option[(GameState, List[GameEvent], Int)] = None

    while attempt < MaxAttempts do
      val shuffler = Shuffler.random
      val deckObj  = Dealing.initialDeckForPlayers(numPlayers, shuffler)
      val deck = deckObj match
           case ListDeck(cs) => cs
      val st0 = GameState.initial(players = numPlayers, playerNames = names, shuffled = deck)
      Engine.step(st0, GameCommand.Deal) match
        case Right((st1, evs)) =>
          if !hasAnyQuartet(st1) then return (st1, evs, deck.size)
          else if lastGood.isEmpty then lastGood = Some((st1, evs, deck.size))
        case Left(_) => ()
      attempt += 1

    lastGood.get

  private def hasAnyQuartet(st: GameState): Boolean =
    st.hands.values.exists { h =>
      h.cards.groupBy(_.rank).values.exists(_.size == 4)
    }
