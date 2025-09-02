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
      case "quit" :: _    => cli.quit()
      case "new"  :: _    => cli.startNewGame()
      case "bot"  :: _    => cli.startNewGameVSBot()
      case "help" :: _    => CLIPrinter.printHelp(cli.currentState.isDefined)
      case extra =>
        cli.currentState match
          case Some(state) =>
            extra match
              case "status" :: _      => CLIPrinter.printStatus(state)
              case "call"   :: _      => cli.step(state, GameCommand.CallBluff(state.turn))
              case "play"   :: tokens => handlePlay(cli, state, tokens)
              case _ =>
                println(s"Comando sconosciuto o sintassi errata: $input")
          case None =>
            println("Al momento non stai giocando. Usa 'new' per cominciare una nuova partita.")

  private def handlePlay(cli: CLI.type, st: GameState, token: List[String]): Unit =
    parsePairs(token) match
      case Left(err) => println(err)
      case Right(Nil) => println("Devi specificare almeno una coppia <quantità rango>, es: play 2 asso")
      case Right(pair) =>
        val declared = st.fixedDeclaredRank.getOrElse(pair.head._2)
        pickCardsByNeed(st, st.turn, pair) match
          case Left(err) => println(err)
          case Right(cards) => cli.step(st, Play(st.turn, cards, declared))

  private def pickCardsByNeed(state: GameState, pid: PlayerId, pairs: List[(Int, Rank)]): Either[String, List[Card]] =
    val hand = state.hands.getOrElse(pid, Hand.empty).cards
    val groupedHand = hand.groupBy(_.rank)
    val lacking = pairs.collect {
      case (q, rk) if groupedHand.getOrElse(rk, Nil).size < q => s"$q x $rk"
    }
    lacking match
      case l if l.nonEmpty => Left(s"Non possiedi abbastanza carte per: ${l.mkString(", ")}")
      case _               =>
        val picked = pairs.flatMap { case (q, rk) => groupedHand(rk).take(q) }
        Right(picked)

  private def parsePairs(tokens: List[String]): Either[String, List[(Int, Rank)]] =
    tokens match
     case Nil => Right(Nil)
     case _ if tokens.length % 2 != 0 =>
        Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
     case _ =>
      tokens.grouped(2).toList.map { g =>
        if (g.size == 2) {
          val qStr = g(0)
          val rankStr = g(1)
          qStr.toIntOption match {
            case None => Left(s"Quantità non valida: $qStr")
            case Some(q) if q <= 0 => Left(s"Quantità non valida: $qStr (deve essere > 0)")
            case Some(q) => CLIPrinter.parseRank(rankStr).map(rk => (q, rk))
          }
        } else Left(s"Errore interno nel parsing dei token: ${g.mkString(" ")}")
      }.foldRight[Either[String, List[(Int, Rank)]]](Right(Nil)) {
        case (Right(pair), Right(acc)) => Right(pair :: acc)
        case (Left(err), _) => Left(err)
        case (_, Left(err)) => Left(err)
      }
