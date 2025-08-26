package it.unibo.bluff.view.cli

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.engine.Engine.GameCommand.Play
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
            case "play" :: str => handlePlayCommand(cli, st, str)
            case _ =>
              println(s"Comando sconosciuto o sintassi errata: $input")

  private def printHelp(gameActive: Boolean): Unit =
    val base = "Comandi: new | help | quit"
    val extra = if gameActive then " | deal | play <cards> as <declRank> <n> | call | hand | pile | status" else ""
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

  private def handlePlay(cli: CLI.type, st: GameState, cardsToPlayStr: List[String], declStr: String, n: Int): Unit =
    val declaredRank = CLIPrinter.parseRank(declStr)
    val cardsToplayRank = parseCards(cardsToPlayStr, st.turn, st)

    (declaredRank, cardsToplayRank) match
      case (Right(decl), Right(cardsToPlay)) =>
        if cardsToPlay.size !=n then
          println(s"Hai indicato ${cardsToPlay.size} carte ma dovevi giocarne $n")
        else
          cli.step(st, Play(st.turn, cardsToPlay, decl))
      case (Left(err), _) => println(err)
      case (_, Left(err)) => println(err)



  private def handlePlayCommand(cli: CLI.type, st: GameState, tokens: List[String]): Unit =
    val (cardsTokens, afterAs) = tokens.span(_ != "as")

    afterAs match
      case "as" :: declRank :: nStr :: Nil =>
        val n = nStr.toIntOption.getOrElse(1)
        handlePlay(cli, st, cardsTokens, declRank, n)
      case _ =>
        println("Sintassi errata. Uso: play <cards> as <declRank> <n>")

  private def parseCards(tokens: List[String], turn: PlayerId, st: GameState): Either[String, List[Card]] =
    val hand = st.hands.getOrElse(turn, Hand.empty).cards

    val ranksE : Either[String, List[Rank]] =
      tokens.foldLeft(Right(Nil): Either[String, List[Rank]]) { (acc, token) =>
        acc.flatMap(lst => CLIPrinter.parseRank(token).map(r => lst :+ r))
      }

    ranksE.flatMap { ranks =>
      val missing = ranks.diff(hand.map(_.rank))
      if missing.nonEmpty then Left(s"Non possiedi le seguenti carte: ${missing.mkString(", ")}")
      else
        Right(ranks.map{ r=>
          val idx =hand.indexWhere(_.rank == r)
          hand(idx)
        })

    }