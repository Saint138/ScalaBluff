package it.unibo.bluff.view.cli

import it.unibo.bluff.controller.GameController
import it.unibo.bluff.model.bot.{BotFactory, BotManager, BotRunner}
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.core.engine.Engine.GameCommand.Play
import it.unibo.bluff.model.core.setup.GameSetup
import it.unibo.bluff.model.*
import it.unibo.bluff.model.cards.{Card, Rank}
import it.unibo.bluff.model.core.state.GameState

import java.util.concurrent.atomic.AtomicReference


class CLICommandHandler:
  private val view = new CLIPrinter()
  private val gameController = new GameController()
  private val stateRef = new AtomicReference[GameState]()
  private var botRunner: Option[BotRunner] = None
  private var running = false
  private var gameEnded = false

  private val BotPollIntervalMillis: Long = 250L
  private val TokenPairSize: Int = 2
  private val MinimumQuantity: Int = 0
  private val BotName: String = "Bot"

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

  BotManager.onEvents = evs => {
    gameController.currentState.foreach { s =>
      view.printEvents(evs, s)
      if evs.exists {
        case GameEvent.GameEnded(_) => true
        case _ => false
      } then
        gameEnded = true
        stopBot()
      val humanId = s.players.find(pid => !s.nameOf(pid).equalsIgnoreCase(BotName))
      humanId.foreach { hId =>
        if s.turn == hId then
          view.printStatus(s)
          view.printHand(s)
      }
    }
  }

  def repl(): Unit =
    running = true
    view.printWelcome()

    while running do
      view.printPrompt()
      val input = view.readInput()
      handleCommand(input)



  private def handleCommand(input: String): Unit =
    input.split("\\s+").toList match
      case Nil | List("") => ()
      case "quit" :: _ => quit()
      case "new" :: _ => startNewGame()
      case "bot" :: botType :: _ => startGameVsBot(botType)
      case "bot" :: Nil => startGameVsBot()
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

  private def handlePlay(state: GameState, tokens: List[String]): Unit =
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
      case Left(err) =>
        println(s"[executeGameCommand] Errore: $err")
        view.printError(err)
      case Right(events) =>
        gameController.currentState.foreach { st =>
          stateRef.set(st)
          view.printEvents(events, st)
          if events.exists {
            case GameEvent.GameEnded(_) => true
            case _ => false
          } then
            gameEnded = true
            stopBot()
          else
            view.printStatus(st)
            view.printHand(st)
        }



  private def startNewGame(): Unit =
    gameEnded = false
    val numPlayers = view.promptPlayersCount()
    val names = view.promptPlayersName(numPlayers)
    
    val (st, events, deckSize) = GameSetup.fairInitialDeal(numPlayers, names)
    gameController.setInitialState(st)
    stateRef.set(st)

    view.printGameStarted(numPlayers, deckSize, st.nameOf(st.turn))
    view.printEvents(events, st)
    view.printStatus(st)
    view.printHand(st)

  private def startGameVsBot(botType: String = "facile"): Unit =
    gameEnded = false
    val selectedBotType = botType.toLowerCase match
      case "medio" => "medio"
      case "difficile" => "difficile"
      case _ => "facile"
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

  private def startBot(st: GameState, botKind: String): Unit =
    stopBot()
    val botId = st.players.find(pid => st.nameOf(pid).equalsIgnoreCase(BotName)).getOrElse(st.players.last)
    val bot = BotFactory(botKind, botId)
    val runner = new BotRunner(
      stateRef = stateRef,
      bot = bot,
      pollMillis = BotPollIntervalMillis,
      onNewState = s => {
        gameController.setCurrentState(s)
        gameController.currentState.foreach(stateRef.set)
      },
      onGameEnded = () => {
        gameEnded = true
        stopBot()
      }
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

  private def pickCardsByNeed(state: GameState, pid: PlayerId, pairs: List[(Int, Rank)]): Either[String, List[Card]] =
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
      case _ if tokens.length % TokenPairSize != 0 =>
        Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
      case _ =>
        tokens.grouped(TokenPairSize).toList.map { g =>
          if g.size == TokenPairSize then
            val qStr = g.head
            val rankStr = g(1)
            qStr.toIntOption match
              case None => Left(s"Quantità non valida: $qStr")
              case Some(q) if q <= MinimumQuantity => Left(s"Quantità non valida: $qStr (deve essere > 0)")
              case Some(q) => view.parseRank(rankStr).map(rk => (q, rk))
          else Left(s"Errore interno nel parsing dei token: ${g.mkString(" ")}")
        }.foldRight[Either[String, List[(Int, Rank)]]](Right(Nil)) {
          case (Right(pair), Right(acc)) => Right(pair :: acc)
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
        }
