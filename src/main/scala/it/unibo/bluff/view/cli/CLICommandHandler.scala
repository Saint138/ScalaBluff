package it.unibo.bluff.view.cli

import it.unibo.bluff.controller.{GameController, RoundManager}
import it.unibo.bluff.model.bot.{BotFactory, BotManager, BotRunner}
import it.unibo.bluff.model.core.engine.Engine.GameCommand
import it.unibo.bluff.model.core.engine.Engine.GameCommand.Play
import it.unibo.bluff.model.core.setup.GameSetup
import it.unibo.bluff.model.*
import it.unibo.bluff.model.cards.{Card, Rank}
import java.util.concurrent.atomic.AtomicReference


class CLICommandHandler:
  private val view = new CLIPrinter()
  private val gameController = new GameController()
  private val stateRef = new AtomicReference[it.unibo.bluff.model.core.state.GameState]()
  private var botRunner: Option[BotRunner] = None
  private var running = false


  BotManager.executeCommand = cmd =>
    gameController.handleCommand(cmd).map { evs =>
      val st2 = gameController.currentState.getOrElse(
        throw new IllegalStateException("No state available after command")
      )
      (st2, evs)
    }

  BotManager.onStateUpdate = s => {
    gameController.setCurrentState(s)
    gameController.currentState.foreach(stateRef.set)
  }

  def repl(): Unit =
    running = true
    view.printWelcome()

    while running do
      view.printPrompt()
      val input = view.readInput()
      handleCommand(input)

      gameController.botTurn() match
        case Right(events) if events.nonEmpty =>
          gameController.currentState.foreach { st =>
            view.printEvents(events, st)
            view.printStatus(st)
            view.printHand(st)
          }
        case Left(err) => view.printError(s"Errore bot: $err")
        case _ => ()

  private def handleCommand(input: String): Unit =
    input.split("\\s+").toList match
      case Nil | List("") => ()
      case "quit" :: _ => quit()
      case "new" :: _ => startNewGame()
      case "bot" :: botType :: _ => startGameVsBot(botType)
      case "bot" :: Nil => startGameVsBot("random")
      case "help" :: _ => view.printHelp(gameController.currentState.isDefined)
      case extra => handleGameCommand(extra)

  private def handleGameCommand(tokens: List[String]): Unit =
    gameController.currentState match
      case Some(state) =>
        tokens match
          case "status" :: _ =>
            view.printStatus(state)
            view.printHand(state)
          case "call" :: _ => executeGameCommand(GameCommand.CallBluff(state.turn))
          case "play" :: playTokens => handlePlay(state, playTokens)
          case _ => view.printUnknownCommand(tokens.mkString(" "))
      case None => view.printNoGameActive()

  private def handlePlay(state: it.unibo.bluff.model.core.state.GameState, tokens: List[String]): Unit =
    parsePairs(tokens) match
      case Left(err) => view.printError(err)
      case Right(Nil) => view.printError("Devi specificare almeno una coppia <quantità rango>, es: play 2 asso")
      case Right(pairs) =>
        val declared = state.fixedDeclaredRank.getOrElse(pairs.head._2)
        pickCardsByNeed(state, state.turn, pairs) match
          case Left(err) => view.printError(err)
          case Right(cards) => executeGameCommand(Play(state.turn, cards, declared))

  private def executeGameCommand(cmd: GameCommand): Unit =
    gameController.handleCommand(cmd) match
      case Left(err) => view.printError(err)
      case Right(events) =>
        gameController.currentState.foreach { st =>
          view.printEvents(events, st)
          view.printStatus(st)
          view.printHand(st)
        }

  private def startNewGame(): Unit =
    val numPlayers = view.promptPlayersCount()
    val names = view.promptPlayersName(numPlayers)
    
    val (st, events, deckSize) = GameSetup.fairInitialDeal(numPlayers, names)
    gameController.setInitialState(st)
    stateRef.set(st)

    view.printGameStarted(numPlayers, deckSize, st.nameOf(st.turn))
    view.printEvents(events, st)
    view.printStatus(st)
    view.printHand(st)

  private def startGameVsBot(botType: String = "random"): Unit =
    val selectedBotType = if botType == "smart" then "smart" else "random"
    val playerName = view.promptPlayersName(1).head
    val names = Vector(playerName, "Bot")
    
    val (st, events, deckSize) = GameSetup.fairInitialDeal(2, names)
    gameController.setInitialState(st)
    stateRef.set(st)
    
    startBot(st, selectedBotType)

    view.printGameStartedVsBot(deckSize, st.nameOf(st.turn), selectedBotType)
    view.printEvents(events, st)
    view.printStatus(st)
    view.printHand(st)

  private def startBot(st: it.unibo.bluff.model.core.state.GameState, botKind: String): Unit =
    stopBot()
    val botId = st.players.find(pid => st.nameOf(pid).equalsIgnoreCase("Bot")).getOrElse(st.players.last)
    val bot = BotFactory(botKind, botId)
    val runner = new BotRunner(
      stateRef = stateRef,
      bot = bot,
      pollMillis = 250L,
      onNewState = s => {
        gameController.setCurrentState(s)
        gameController.currentState.foreach(stateRef.set)
      },
      onGameEnded = () => ()
    )
    botRunner = Some(runner)
    runner.start()

  private def stopBot(): Unit =
    botRunner.foreach(_.stop())
    botRunner = None

  private def quit(): Unit =
    running = false
    stopBot()
    view.printGoodbye()

  private def pickCardsByNeed(state: it.unibo.bluff.model.core.state.GameState, pid: PlayerId, pairs: List[(Int, Rank)]): Either[String, List[Card]] =
    val hand = state.hands.getOrElse(pid, Hand.empty).cards
    val groupedHand = hand.groupBy(_.rank)
    val lacking = pairs.collect {
      case (q, rk) if groupedHand.getOrElse(rk, Nil).size < q => s"$q x $rk"
    }
    lacking match
      case l if l.nonEmpty => Left(s"Non possiedi abbastanza carte per: ${l.mkString(", ")}")
      case _ =>
        val picked = pairs.flatMap { case (q, rk) => groupedHand(rk).take(q) }
        Right(picked)

  private def parsePairs(tokens: List[String]): Either[String, List[(Int, Rank)]] =
    tokens match
      case Nil => Right(Nil)
      case _ if tokens.length % 2 != 0 =>
        Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
      case _ =>
        tokens.grouped(2).toList.map { g =>
          if g.size == 2 then
            val qStr = g(0)
            val rankStr = g(1)
            qStr.toIntOption match
              case None => Left(s"Quantità non valida: $qStr")
              case Some(q) if q <= 0 => Left(s"Quantità non valida: $qStr (deve essere > 0)")
              case Some(q) => view.parseRank(rankStr).map(rk => (q, rk))
          else Left(s"Errore interno nel parsing dei token: ${g.mkString(" ")}")
        }.foldRight[Either[String, List[(Int, Rank)]]](Right(Nil)) {
          case (Right(pair), Right(acc)) => Right(pair :: acc)
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
        }
