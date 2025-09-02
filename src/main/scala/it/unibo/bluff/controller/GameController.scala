package it.unibo.bluff.controller

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.bot.RandomBot
import it.unibo.bluff.model.bot.BotManager
import it.unibo.bluff.model.TurnOrder.given

class GameController:

  private var state: Option[GameState] = None
  private var bot: Option[RandomBot] = None

  def currentState: Option[GameState] = state
  def setGameState(st: GameState): Unit = state = Some(st)
  def currentBot: Option[RandomBot] = bot

  def newGame(numPlayers: Int, names: Vector[String], deck: List[Card]): Either[String, List[GameEvent]] =
    val st0 = GameState.initial(numPlayers, names, deck)
    Engine.step(st0, GameCommand.Deal).map { case (st1, evs) =>
      state = Some(st1)
      bot = None
      evs
    }

  def newGameVSBot(): Either[String, List[GameEvent]] =
    val numPlayers = 2
    val names = Vector("Human", "Bot")
    val deck = Dealing.initialDeckForPlayers(numPlayers, Shuffler.random) match
      case ListDeck(cs) => cs
    val st0 = GameState.initial(numPlayers, names, deck)
    Engine.step(st0, GameCommand.Deal).map { case (st1, evs) =>
      state = Some(st1)
      bot = Some(RandomBot(PlayerId(1)))
      evs
    }

  def handleCommand(cmd: GameCommand): Either[String, List[GameEvent]] =
    state match
      case None => Left("Nessuna partita in corso")
      case Some(st) =>
        Engine.step(st, cmd).map { case (st2, evs) =>
          state = Some(st2)
          evs
        }

  def botTurn(): Either[String, List[GameEvent]] =
    (state, bot) match
      case (Some(st), Some(b)) if st.turn == b.id =>
        BotManager.takeTurn(b, st).map { case (newSt, evs) =>
          state = Some(newSt)
          evs
        }
      case _ => Right(Nil)
