package it.unibo.bluff.view.cli

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState

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
        // Stampa eventi e stato aVggiornato (passo st2 per i nomi)
        CLIPrinter.printEvents(events, st2)
        CLIPrinter.printStatus(st2)
        // Mostra automaticamente la mano del giocatore di turno
        CLIPrinter.printHand(st2)

        // Se vuoi chiudere il REPL a fine partita, decommenta:
        // if events.exists(_.isInstanceOf[GameEvent.GameEnded]) then
        //   println("🏁 Partita terminata. Digita 'new' per iniziare una nuova partita o 'quit' per uscire.")
        //   running = false

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

  /** Inizializza lo stato e distribuisce le carte assicurando: nessun quartetto iniziale in nessuna mano. */
  private def initGame(numPlayers: Int, names: Vector[String]): Unit =
    val (stDealt, dealtEvents, deckSize) = fairInitialDeal(numPlayers, names)

    gameState = Some(stDealt)
    println(s"Nuova partita con $numPlayers giocatori.")
    println(s"Mazzo iniziale: $deckSize carte.")
    println(s"Primo turno: ${stDealt.nameOf(stDealt.turn)}")

    // Stampa la distribuzione accettata (una sola volta)
    CLIPrinter.printEvents(dealtEvents, stDealt)
    CLIPrinter.printStatus(stDealt)
    CLIPrinter.printHand(stDealt)

  /** Loop di deal: re-shuffle finché nessun giocatore ha 4 carte dello stesso rango dopo il deal. */
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
          if !hasAnyQuartet(st1) then
            return (st1, evs, deck.size)
          else
            // tieni traccia di un candidato (non usato se troviamo prima uno valido)
            if lastGood.isEmpty then lastGood = Some((st1, evs, deck.size))
        case Left(err) =>
          // improbabile; riprova un altro shuffle
          ()

      attempt += 1

    // Fallback: in pratica non ci si arriva, ma nel caso restituisci l'ultimo stato generato
    lastGood.get

  /** True se almeno un giocatore ha un quartetto in mano. */
  private def hasAnyQuartet(st: GameState): Boolean =
    st.hands.values.exists { h =>
      h.cards.groupBy(_.rank).values.exists(_.size == 4)
    }
