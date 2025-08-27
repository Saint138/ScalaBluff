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
      case "help" :: _    => printHelp(cli.currentState.isDefined)

      case other =>
        cli.currentState match
          case None =>
            println("Nessuna partita in corso. Usa il comando 'new' per avviarne una")
          case Some(st) =>
            other match
              case "status" :: _      => CLIPrinter.printStatus(st)
              case "call"   :: _      => cli.step(st, GameCommand.CallBluff(st.turn))
              case "play"   :: tokens => handlePlayPairs(cli, st, tokens)
              case _ =>
                println(s"Comando sconosciuto o sintassi errata: $input")

  // --------------------------------------------------------------------

  private def printHelp(gameActive: Boolean): Unit =
    val base  = "Comandi: new | help | quit"
    val extra =
      if gameActive then
        // Unico formato: sequenza di coppie <n rank>
        " | play <n1> <rank1> [<n2> <rank2> ...] | call | status"
      else ""
    println(base + extra)

  /**
   * Nuovo 'play': parse di coppie quantità–rango, selezione carte dalla mano,
   * e scelta del rango dichiarato (first pair se non fissato, altrimenti fixedDeclaredRank).
   */
  private def handlePlayPairs(cli: CLI.type, st: GameState, tokens: List[String]): Unit =
    parsePairs(tokens) match
      case Left(err) =>
        println(err)
      case Right(pairs) =>
        if pairs.isEmpty then
          println("Devi specificare almeno una coppia <quantità rango>, es: play 2 asso")
        else
          // Determina il rango dichiarato
          val declared: Rank =
            st.fixedDeclaredRank.getOrElse(pairs.head._2)

          // Costruisci le richieste per rango: Map[Rank, Int]
          val need: Map[Rank, Int] =
            pairs.groupMapReduce(_._2)(_._1)(_ + _)

          // Seleziona le carte dalla mano consumandole
          pickCardsByNeed(st, st.turn, need) match
            case Left(err) =>
              println(err)
            case Right(cardsToPlay) =>
              cli.step(st, Play(st.turn, cardsToPlay, declared))

  /** Parsing coppie <quantità rango>: es. ["1","asso","2","due",...] -> List((1,Asso),(2,Due),...) */
  private def parsePairs(tokens: List[String]): Either[String, List[(Int, Rank)]] =
    if tokens.isEmpty then Right(Nil)
    else if tokens.length % 2 != 0 then
      Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
    else
      // crea coppie (q, rank) in ordine
      val grouped = tokens.grouped(2).toList
      val parsed: List[Either[String, (Int, Rank)]] = grouped.map {
        case qStr :: rankStr :: Nil =>
          val qOpt = qStr.toIntOption
          (qOpt, CLIPrinter.parseRank(rankStr)) match
            case (Some(q), Right(rk)) if q > 0 => Right((q, rk))
            case (Some(_), Right(_))           => Left(s"Quantità non valida: $qStr (deve essere > 0)")
            case (None, _)                     => Left(s"Quantità non valida: $qStr")
            case (_, Left(e))                  => Left(e)
        case _ =>
          Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
      }

      // accumula errori o ritorna i pairs
      parsed.foldLeft(Right(List.empty): Either[String, List[(Int, Rank)]]) {
        case (Right(acc), Right(p)) => Right(acc :+ p)
        case (Left(err), _)         => Left(err)
        case (_, Left(err))         => Left(err)
      }

  /**
   * Seleziona dalla mano le carte richieste da `need` (Map[Rank, Int]),
   * consumandole per evitare duplicati. Ritorna le carte scelte nell'ordine specificato da `need` (aggregato).
   */
  private def pickCardsByNeed(st: GameState, pid: PlayerId, need: Map[Rank, Int]): Either[String, List[Card]] =
    val hand = st.hands.getOrElse(pid, Hand.empty).cards

    // Controllo disponibilità
    val have = hand.groupBy(_.rank).view.mapValues(_.size).toMap
    val lacking = need.collect { case (rk, q) if have.getOrElse(rk, 0) < q => s"$q x $rk" }
    if lacking.nonEmpty then
      Left(s"Non possiedi abbastanza carte per: ${lacking.mkString(", ")}")
    else
      // Consumo: per ogni rango prendo q carte e le tolgo dalla pool
      var pool = hand
      val picked = scala.collection.mutable.ListBuffer.empty[Card]
      // Per stabilità, rispettiamo l'ordine di inserimento (non richiesto, ma piacevole)
      for (rk, q) <- need.toList do
        var remaining = q
        while remaining > 0 do
          val idx = pool.indexWhere(_.rank == rk)
          picked += pool(idx)
          pool = pool.take(idx) ++ pool.drop(idx + 1)
          remaining -= 1
      Right(picked.toList)
