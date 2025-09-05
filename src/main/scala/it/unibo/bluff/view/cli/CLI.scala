package it.unibo.bluff.view.cli

import it.unibo.bluff.controller.GameController
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
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
      // turno eventuale “bot” (attualmente no-op: GameController.botTurn => Right(Nil))
      controller.botTurn() match
        case Right(events) if events.nonEmpty =>
          controller.currentState.foreach { st =>
            CLIPrinter.printEvents(events, st)
            CLIPrinter.printStatus(st)
            CLIPrinter.printHand(st)
          }
        case Left(err) => println(s"Errore bot: $err")
        case _ => ()

  def quit(): Unit =
    running = false
    println("Arrivederci!")

  def currentState: Option[GameState] = controller.currentState

  // -------------------- Avvio partite --------------------

  def startNewGame(): Unit =
    val numPlayers = promptPlayersCount()
    val names = promptPlayersName(numPlayers)
    val (st, evs, deckSize) = fairInitialDeal(numPlayers, names)
    controller.setGameState(st)
    println(s"Nuova partita con $numPlayers giocatori.")
    println(s"Mazzo iniziale: $deckSize carte.")
    println(s"Primo turno: ${st.nameOf(st.turn)}")
    CLIPrinter.printEvents(evs, st)
    CLIPrinter.printStatus(st)
    CLIPrinter.printHand(st)

  def startNewGameVSBot(): Unit =
    // per ora: 2 giocatori "Human" e "Bot" (nessuna IA automatica lato CLI)
    val (st, evs, deckSize) = fairInitialDeal(2, Vector("Human", "Bot"))
    controller.setGameState(st)
    println("Nuova partita contro il Bot!")
    println(s"Mazzo iniziale: $deckSize carte.")
    println(s"Primo turno: ${st.nameOf(st.turn)}")
    CLIPrinter.printEvents(evs, st)
    CLIPrinter.printStatus(st)
    CLIPrinter.printHand(st)

  /** Esegue un passo dell'engine tramite Controller e stampa effetti. */
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
      val name = StdIn.readLine().trim
      if name.isEmpty then s"Player${i+1}" else name
    }.toVector

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

    // se non troviamo una distribuzione perfetta, torniamo l'ultima valida
    lastGood.get

  private def hasAnyQuartet(st: GameState): Boolean =
    st.hands.values.exists { h =>
      h.cards.groupBy(_.rank).values.exists(_.size == 4)
    }
