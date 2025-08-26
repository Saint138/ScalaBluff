package it.unibo.bluff.view.cli


import it.unibo.bluff.model.*
import it.unibo.bluff.engine.Engine.GameEvent
import it.unibo.bluff.model.state.GameState

object  CLIPrinter:

  def printStatus(st: GameState): Unit =
    val handsSizes = st.hands.toSeq.sortBy(_._1.value).map { case (pid, h) => s"${pid.value}:${h.size}" }.mkString(", ")
    val pileSize = st.pile.allCards.size
    val lastDecl = st.lastDeclaration.map(d => s"${d.player.value}→${d.declared} (${d.hiddenCards.size})").getOrElse("-")
    println(
      s"""Stato:
         |  Turno: ${st.nameOf(st.turn)}
         |  Carte in mano: [$handsSizes]
         |  Pila centrale: $pileSize carte
         |  Ultima dichiarazione: $lastDecl
         |""".stripMargin.trim)

  def printHand(st: GameState): Unit =
    val hand = st.hands.getOrElse(st.turn, Hand.empty).cards
    val byRank = hand.groupBy(_.rank).toSeq.sortBy(_._1.ordinal).map { case (r, cs) => s"$r:${cs.size}" }.mkString(", ")
    println(s"Giocatore ${st.nameOf(st.turn)} – carte per rango: $byRank")


  def printPile(st: GameState): Unit =
    println(s"Pila: ${st.pile.allCards.size} carte totali")

  def printEvents(events: Seq[GameEvent]): Unit =
    events.foreach {
      case GameEvent.Dealt(sizes) =>
        val pretty = sizes.toSeq.sortBy(_._1.value).map { case (pid, sz) => s"${pid.value}:$sz" }.mkString(", ")
        println(s"Event: carte distribuite → [$pretty]")
      case GameEvent.Played(player, declared, count) =>
        println(s"Event: player ${player.value} dichiara $declared e gioca $count carte")
      case GameEvent.BluffCalled(by, against, truthful) =>
        val esito = if truthful then "VERA" else "FALSA"
        println(s"Event: accusa di bluff da ${by.value} contro ${against.player.value} → dichiarazione $esito")
    }

  def parseRank(s: String): Either[String, Rank] =
    val norm = s.trim.toLowerCase
    val mapping: Map[String, Rank] = Map(
      "a" -> Rank.Ace, "ace" -> Rank.Ace,
      "k" -> Rank.King, "king" -> Rank.King,
      "q" -> Rank.Queen, "queen" -> Rank.Queen,
      "j" -> Rank.Jack, "jack" -> Rank.Jack,
      "10" -> Rank.Ten, "t" -> Rank.Ten,
      "9" -> Rank.Nine, "8" -> Rank.Eight, "7" -> Rank.Seven,
      "6" -> Rank.Six, "5" -> Rank.Five, "4" -> Rank.Four,
      "3" -> Rank.Three, "2" -> Rank.Two
    )
    mapping.get(norm).toRight(s"Rank non riconosciuto: $s")