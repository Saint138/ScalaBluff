package it.unibo.bluff.model.bot

import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.core.state.GameState

/*Manages bot turns and integrates their moves with the game engine, updating state and events.*/

object BotManager:

  @volatile var onEvents: List[GameEvent] => Unit = _ => ()
  @volatile var onStateUpdate: GameState => Unit = _ => ()
  @volatile var executeCommand: GameCommand => Either[String, (GameState, List[GameEvent])] = _ => Right((null, Nil))

  def takeTurn(bot: Bot, state: GameState): Either[String, (GameState, List[GameEvent])] =
    val move   = bot.decideMove(state)
    executeCommand(move).map { case (st2, evs) =>
      val finalEvents = adaptEvents(move, evs)
      onStateUpdate(st2)
      onEvents(finalEvents)
      (st2, finalEvents)
    }

  private def adaptEvents(move: GameCommand, evs: List[GameEvent]): List[GameEvent] =
    move match
      case play: GameCommand.Play =>
        val evsNoPlayed = evs.filter {
          case GameEvent.Played(p, _, _) if p == play.player => false
          case _ => true
        }
        val botEv = GameEvent.BotPlayed(play.player, play.declared, play.cards.size)
        val updated = evsNoPlayed.flatMap {
          case ge @ GameEvent.GameEnded(_) => Seq(botEv, ge)
          case other => Seq(other)
        }
        if !updated.contains(botEv) then updated :+ botEv else updated
      case _ => evs
