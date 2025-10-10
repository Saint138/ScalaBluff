package it.unibo.bluff.view.cli

import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.GameEvent
import it.unibo.bluff.model.cards.Rank
import it.unibo.bluff.model.*
import scala.io.StdIn

/** View della CLI - gestisce solo input/output, delega tutto ai controller */
class CLIPrinter:
  def printWelcome(): Unit =
    println("=== ScalaBluff CLI ===")
    println("Comandi: new | bot <tipo> | help | quit")

  def printPrompt(): Unit = print("> ")

  def readInput(): String = Option(StdIn.readLine()).getOrElse("").trim

  def printHelp(gameActive: Boolean): Unit =
    val base = "Comandi: new | bot <tipo> | help | quit"
    val extra = if gameActive then " | play <n1> <rank1> [<n2> <rank2> ...] | call | status" else ""
    println(base + extra)
    if !gameActive then
      println("Tipi di bot disponibili: facile, medio, difficile")

  def printGameStarted(numPlayers: Int, deckSize: Int, firstPlayer: String): Unit =
    println(s"Nuova partita con $numPlayers giocatori.")
    println(s"Mazzo iniziale: $deckSize carte.")
    println(s"Primo turno: $firstPlayer")

  def printGameStartedVsBot(deckSize: Int, firstPlayer: String, botKind: String): Unit =
    println(s"Nuova partita contro il Bot ($botKind)!")
    println(s"Mazzo iniziale: $deckSize carte.")
    println(s"Primo turno: $firstPlayer")

  def printError(error: String): Unit =
    println(s"Errore: $error")

  def printUnknownCommand(input: String): Unit =
    println(s"Comando sconosciuto o sintassi errata: $input")

  def printNoGameActive(): Unit =
    println("Al momento non stai giocando. Usa 'new' per cominciare una nuova partita.")

  def printGoodbye(): Unit =
    println("Arrivederci!")

  def printEvents(events: Seq[GameEvent], st: GameState): Unit =
    events.foreach {
      case GameEvent.Dealt(sizes) =>
        val pretty = sizes.toSeq
          .sortBy(_._1.value)
          .map { case (pid, sz) => s"${st.nameOf(pid)}:$sz" }
          .mkString(", ")
        println(s"Event: carte distribuite [$pretty]")
      case GameEvent.Played(player, declared, count) =>
        println(s"Event: ${st.nameOf(player)} dichiara $declared e gioca $count carte")
      case GameEvent.BotPlayed(player, declared, count) =>
        println(s"Event: (BOT) ${st.nameOf(player)} dichiara $declared e gioca $count carte")
      case GameEvent.BluffCalled(by, against, truthful) =>
        val esito = if truthful then "VERA" else "FALSA"
        println(s"Event: accusa di bluff da ${st.nameOf(by)} contro ${st.nameOf(against.player)} → dichiarazione $esito")
      case GameEvent.GameEnded(winner) =>
        println(s"🏆 Vince ${st.nameOf(winner)}!")
      case GameEvent.TimerExpired(player) =>
        println(s"Event: timeout di ${st.nameOf(player)}")
      case GameEvent.QuartetCleared(player, rank, count) =>
      println(s"Event: ♻️ ${st.nameOf(player)} elimina automaticamente $count carte ($rank)")
    }

  def printStatus(st: GameState): Unit =
    val handsSizes = st.hands.toSeq
      .sortBy(_._1.value)
      .map { case (pid, h) => s"${st.nameOf(pid)}:${h.size}" }
      .mkString(", ")
    val pileSize = st.pile.allCards.size
    val lastDecl = st.lastDeclaration.map(d => s"${st.nameOf(d.player)} -> ${d.declared} (${d.hiddenCards.size})").getOrElse("-")
    println(
      s"""Stato:
         |  Turno: ${st.nameOf(st.turn)}
         |  Carte in mano: [$handsSizes]
         |  Pila centrale: $pileSize carte
         |  Ultima dichiarazione: $lastDecl
         |""".stripMargin.trim
    )

  def printHand(st: GameState): Unit =
    val hand = st.hands.getOrElse(st.turn, Hand.empty).cards
    val byRank = hand.groupBy(_.rank).toSeq.sortBy(_._1.ordinal).map {
      case (r, cs) => s"$r:${cs.size}"
    }.mkString(", ")
    println(s"Giocatore ${st.nameOf(st.turn)} - carte per rango: $byRank")

  def promptPlayersCount(): Int =
    var count = 0
    while count < 2 || count > 4 do
      print("Inserisci il numero di giocatori (2-4): ")
      count = StdIn.readLine().toIntOption.getOrElse(0)
      if count < 2 || count > 4 then println("Numero non valido.")
    count

  def promptPlayersName(players: Int): Vector[String] =
    (0 until players).map { i =>
      print(s"Inserisci il nome per il giocatore ${i + 1}: ")
      val name = StdIn.readLine().trim
      if name.isEmpty then s"Player${i+1}" else name
    }.toVector

  def promptBotType(): String =
    print("Scegli il tipo di bot (random/smart) [default: random]: ")
    val input = StdIn.readLine().trim.toLowerCase
    if input == "smart" then "smart" else "random"

  /** Parsing rank in italiano con alias (case-insensitive) */
  def parseRank(s: String): Either[String, Rank] =
    val norm = s.trim.toLowerCase
    val mapping: Map[String, Rank] = Map(
      "a" -> Rank.Asso, "asso" -> Rank.Asso,
      "k" -> Rank.King, "re" -> Rank.King, "king" -> Rank.King,
      "q" -> Rank.Queen, "donna" -> Rank.Queen, "queen" -> Rank.Queen,
      "j" -> Rank.Jack, "fante" -> Rank.Jack, "jack" -> Rank.Jack,
      "10" -> Rank.Dieci, "dieci" -> Rank.Dieci,
      "9" -> Rank.Nove, "nove" -> Rank.Nove,
      "8" -> Rank.Otto, "otto" -> Rank.Otto,
      "7" -> Rank.Sette, "sette" -> Rank.Sette,
      "6" -> Rank.Sei, "sei" -> Rank.Sei,
      "5" -> Rank.Cinque, "cinque" -> Rank.Cinque,
      "4" -> Rank.Quattro, "quattro" -> Rank.Quattro,
      "3" -> Rank.Tre, "tre" -> Rank.Tre,
      "2" -> Rank.Due, "due" -> Rank.Due
    )
    mapping.get(norm).toRight(s"Rank non riconosciuto: $s")
