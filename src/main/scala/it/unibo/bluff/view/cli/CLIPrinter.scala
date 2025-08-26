package it.unibo.bluff.view.cli

import it.unibo.bluff.model.*
import it.unibo.bluff.engine.Engine.GameEvent
import it.unibo.bluff.model.state.GameState

object CLIPrinter:

  /** Stampa lo stato corrente della partita */
  def printStatus(st: GameState): Unit =
    val handsSizes = st.hands.toSeq
      .sortBy(_._1.value)
      .map { case (pid, h) => s"${st.nameOf(pid)}:${h.size}" }
      .mkString(", ")
    val pileSize = st.pile.allCards.size
    val lastDecl = st.lastDeclaration
      .map(d => s"${st.nameOf(d.player)}→${d.declared} (${d.hiddenCards.size})")
      .getOrElse("-")
    println(
      s"""Stato:
         |  Turno: ${st.nameOf(st.turn)}
         |  Carte in mano: [$handsSizes]
         |  Pila centrale: $pileSize carte
         |  Ultima dichiarazione: $lastDecl
         |""".stripMargin.trim
    )

  /** Stampa le carte del giocatore di turno raggruppate per rango */
  def printHand(st: GameState): Unit =
    val hand   = st.hands.getOrElse(st.turn, Hand.empty).cards
    val byRank = hand.groupBy(_.rank).toSeq.sortBy(_._1.ordinal).map {
      case (r, cs) => s"$r:${cs.size}"
    }.mkString(", ")
    println(s"Giocatore ${st.nameOf(st.turn)} – carte per rango: $byRank")

  /** Stampa lo stato della pila centrale */
  def printPile(st: GameState): Unit =
    println(s"Pila: ${st.pile.allCards.size} carte totali")

  /** Stampa la lista di eventi generati da una mossa (usa GameState per i nomi) */
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

      case GameEvent.BluffCalled(by, against, truthful) =>
        val esito = if truthful then "VERA" else "FALSA"
        println(s"Event: accusa di bluff da ${st.nameOf(by)} contro ${st.nameOf(against.player)} → dichiarazione $esito")

      case GameEvent.GameEnded(winner) =>
        println(s"🏆 Vince ${st.nameOf(winner)}!")
    }

  /** Parsing rank in italiano con alias (case-insensitive) */
  def parseRank(s: String): Either[String, Rank] =
    val norm = s.trim.toLowerCase

    // alias utili (lettere, numeri e nomi italiani)
    val mapping: Map[String, Rank] = Map(
      "a" -> Rank.Asso, "asso" -> Rank.Asso,
      "k" -> Rank.King, "re" -> Rank.King, "king" -> Rank.King,
      "q" -> Rank.Queen, "donna" -> Rank.Queen, "queen" -> Rank.Queen,
      "j" -> Rank.Jack, "fante" -> Rank.Jack, "jack" -> Rank.Jack,
      "10" -> Rank.Dieci, "dieci" -> Rank.Dieci,
      "9" -> Rank.Nove,  "nove" -> Rank.Nove,
      "8" -> Rank.Otto,  "otto" -> Rank.Otto,
      "7" -> Rank.Sette, "sette" -> Rank.Sette,
      "6" -> Rank.Sei,   "sei" -> Rank.Sei,
      "5" -> Rank.Cinque,"cinque" -> Rank.Cinque,
      "4" -> Rank.Quattro,"quattro" -> Rank.Quattro,
      "3" -> Rank.Tre,   "tre" -> Rank.Tre,
      "2" -> Rank.Due,   "due" -> Rank.Due
    )

    mapping.get(norm).toRight(s"Rank non riconosciuto: $s")
