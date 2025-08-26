package it.unibo.bluff.view.cli

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.*

object CommandHandler:

  def execute(input: String, cli: CLI.type): Unit =
    input.split("\\s+").toList match
      case Nil | List("") => ()
      case "quit" :: _ => cli.quit()
      case "new" :: _ => cli.startNewGame()
      case "help" :: _ =>  printHelp(cli.currentState.isDefined)
      case other =>
        cli.currentState match
          case None => println("Nessuna partita in corso. Usa il comando 'new' per avviarne una")
          case Some(st) => other match
            case "status" :: _ => CLIPrinter.printStatus(st)
            case "hand" :: _   => CLIPrinter.printHand(st)
            case "pile" :: _   => CLIPrinter.printPile(st)
            case "call" :: _ => cli.step(st, GameCommand.CallBluff(st.turn))
            case "play" :: rankStr :: rest =>
              handlePlay(cli, st, rankStr, rest.headOption.flatMap(_.toIntOption).getOrElse(1))
            case "play-any" :: declStr :: nStr :: Nil =>
              handlePlayAny(cli, st, declStr, nStr.toIntOption.getOrElse(1))
            case _ =>
              println(s"Comando sconosciuto o sintassi errata: $input")

  private def printHelp(gameActive: Boolean): Unit =
    val base = "Comandi: new | help | quit"
    val extra = if gameActive then " | deal | play <rank> [n] | play-any <declRank> <n> | call | hand | pile | status" else ""
    println(base + extra)

  private def handlePlay(cli: CLI.type, st: GameState, rankStr: String, n: Int): Unit =
    CLIPrinter.parseRank(rankStr) match
      case Left(err) => println(err)
      case Right(rank) =>
        val me = st.turn
        val hand = st.hands.getOrElse(me, Hand.empty).cards
        val toPlay = hand.filter(_.rank == rank).take(n)
        if toPlay.size != n then println(s"Non hai $n carte di rango $rank.")
        else cli.step(st, GameCommand.Play(me, toPlay, rank))

  private def handlePlayAny(cli: CLI.type, st: GameState, declStr: String, n: Int): Unit =
    CLIPrinter.parseRank(declStr) match
      case Left(err) => println(err)
      case Right(decl) =>
        val me = st.turn
        val hand = st.hands.getOrElse(me, Hand.empty).cards
        if hand.size < n then println(s"Non hai $n carte da giocare.")
        else cli.step(st, GameCommand.Play(me, hand.take(n), decl))