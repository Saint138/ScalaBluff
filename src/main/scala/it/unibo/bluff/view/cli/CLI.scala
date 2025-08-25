package it.unibo.bluff.view.cli

import scala.io.StdIn

import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*
import it.unibo.bluff.model.util.RNG
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}


object CLI:

  private var gameState: Option[GameState] = None
  private var running = false

  def currentState: Option[GameState] = gameState

  def currentStatus: String = statusMessage()

  def start(players: Int = 2): Unit =
    val rng  = RNG.default()
    val deck = DeckBuilder.standardShuffled(rng)

    val names = (0 until players).map {i =>
      print(s"Inserisci il nome per il giocatore ${i+1}: ")
      scala.io.StdIn.readLine()
    }.toVector

    val st   = GameState.initial(players,names, deck)
    gameState = Some(st)
    println(s"Nuova partita con $players giocatori.")
    println(s"Mazzo iniziale: ${deck.size} carte.")
    println(s"Primo turno: ${st.nameOf(st.turn)}")
    withState(st => step(st, GameCommand.Deal))

  def repl(): Unit =
    print("\n> ")
    println("Comandi: new | help ")

    running = true
    while running do
      print("> ")
      val line = Option(StdIn.readLine()).getOrElse("")
      execute(line)

  def execute(input: String): Unit =
    val toks = input.trim.split("\\s+").toList
    toks match
      case Nil | List("") => ()
      case "quit" :: _ =>
        running = false
      case "help" :: _ =>
        println("Comandi: new | help | quit" + (if gameState.isDefined then " |deal | play <rank> [n] | play-any <declRank> <n> | call | hand | pile | status | quit" else ""))
      case "new" :: _ =>
        var numPlayers: Int = 0
        while numPlayers < 2 || numPlayers > 4 do
           print("Inserisci il numero di giocatori (2-4): ")
           val input = StdIn.readLine()
           try
             numPlayers = input.toInt
             if numPlayers < 2 || numPlayers > 4 then
                  println("Numero non valido, inserisci un numero tra 2 e 4.")
           catch
             case _: NumberFormatException =>
                 println("Input non valido, inserisci un numero tra 2 e 4.")
        start(numPlayers)

      case other =>
        if gameState.isEmpty then
          println ("Nessuna partita in corso. Usa 'new' per iniziare o 'help' per aiuto.")
        else
          other match
            case "status" :: _ => println(statusMessage())
            case "hand" :: _ => printHand()
            case "pile" :: _ => printPile()
            case "call" :: _ => withState(st => step(st, GameCommand.CallBluff(st.turn)))
            case "play" :: rankStr :: rest =>
              withState { st =>
                parseRank(rankStr) match
                  case Left(err) => println(err)
                  case Right(rank) =>
                    val n = rest.headOption.flatMap(_.toIntOption).getOrElse(1)
                    val me = st.turn
                    val hand = st.hands.getOrElse(me, Hand.empty).cards
                    val toPlay = hand.filter(_.rank == rank).take(n)
                    if toPlay.size != n then
                      println(s"Non hai $n carte di rango $rank.")
                    else
                      step(st, GameCommand.Play(me, toPlay, rank))
              }
            case "play-any" :: declStr :: nStr :: Nil =>
              withState { st =>
                parseRank(declStr) match
                  case Left(err) => println(err)
                  case Right(decl) =>
                    val n = nStr.toIntOption.getOrElse(1)
                    val me = st.turn
                    val hand = st.hands.getOrElse(me, Hand.empty).cards
                    if hand.size < n then println(s"Non hai $n carte da giocare.")
                    else
                      val toPlay = hand.take(n)
                      step(st, GameCommand.Play(me, toPlay, decl))
              }
            case "play" :: _ =>
              println("Uso: play <rank> [n]")
            case _ =>
               println(s"Comando sconosciuto: ${other.mkString(" ")}")

      



  // ----- Helpers -----

  private def withState(f: GameState => Unit): Unit =
    gameState match
      case None     => println("Nessuna partita in corso. Usa 'new'.")
      case Some(st) => f(st)

  private def step(st: GameState, cmd: GameCommand): Unit =
    Engine.step(st, cmd) match
      case Left(err) =>
        println(s"Errore: $err")
      case Right((st2, events)) =>
        gameState = Some(st2)
        events.foreach(printEvent)
        println(statusMessage())

  private def printEvent(ev: GameEvent): Unit = ev match
    case GameEvent.Dealt(sizes) =>
      val pretty = sizes.toSeq.sortBy(_._1.value).map{ case (pid, sz) => s"${pid.value}:$sz" }.mkString(", ")
      println(s"Event: carte distribuite → [$pretty]")
    case GameEvent.Played(player, declared, count) =>
      println(s"Event: player ${player.value} dichiara $declared e gioca $count carte")
    case GameEvent.BluffCalled(by, against, truthful) =>
      val esito = if truthful then "VERA" else "FALSA"
      println(s"Event: accusa di bluff da ${by.value} contro ${against.player.value} → dichiarazione $esito")

  private def statusMessage(): String =
    gameState match
      case None => "Nessuna partita in corso."
      case Some(st) =>
        val handsSizes = st.hands.toSeq.sortBy(_._1.value).map{ case (pid,h) => s"${pid.value}:${h.size}" }.mkString(", ")
        val pileSize   = st.pile.allCards.size
        val lastDecl   = st.lastDeclaration.map(d => s"${d.player.value}→${d.declared} (${d.hiddenCards.size})").getOrElse("-")
        s"""Stato:
           |  Turno: ${st.nameOf(st.turn)}
           |  Carte in mano: [$handsSizes]
           |  Pila centrale: $pileSize carte
           |  Ultima dichiarazione: $lastDecl
           |""".stripMargin.trim

  private def printHand(): Unit =
    gameState match
      case None => println("Nessuna partita in corso.")
      case Some(st) =>
        val hand = st.hands.getOrElse(st.turn, Hand.empty).cards
        val byRank = hand.groupBy(_.rank).toSeq.sortBy(_._1.ordinal).map{ case (r, cs) => s"$r:${cs.size}"}.mkString(", ")
        println(s"Giocatore ${st.nameOf(st.turn)} – carte per rango: $byRank")

  private def printPile(): Unit =
    gameState match
      case None => println("Nessuna partita in corso.")
      case Some(st) =>
        println(s"Pila: ${st.pile.allCards.size} carte totali")

  private def turnString(st: GameState): String =
    s"${st.turn.value}"

  // Adatta i nomi dei rank al tuo enum se servisse
  private def parseRank(s: String): Either[String, Rank] =
    val norm = s.trim.toLowerCase
    val map: Map[String, Rank] =
      Map(
        "a" -> Rank.Ace, "ace" -> Rank.Ace,
        "k" -> Rank.King, "king" -> Rank.King,
        "q" -> Rank.Queen, "queen" -> Rank.Queen,
        "j" -> Rank.Jack, "jack" -> Rank.Jack,
        "10" -> Rank.Ten, "t" -> Rank.Ten,
        "9" -> Rank.Nine, "8" -> Rank.Eight, "7" -> Rank.Seven,
        "6" -> Rank.Six, "5" -> Rank.Five, "4" -> Rank.Four,
        "3" -> Rank.Three, "2" -> Rank.Two
      )
    map.get(norm).toRight(s"Rank non riconosciuto: $s")
