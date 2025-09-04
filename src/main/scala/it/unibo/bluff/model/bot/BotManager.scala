package it.unibo.bluff.model.bot

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameEvent
import it.unibo.bluff.model.state.GameState
object BotManager:

  /** Esegue il turno del bot */
  def takeTurn(bot: RandomBot, state: GameState): Either[String, (GameState, List[GameEvent])] =
    val move = bot.decideMove(state)

    // keep terminal prints as you requested
    move match
      case play: Engine.GameCommand.Play =>
        println(s"🤖 Bot gioca: ${play.cards.map(_.rank).mkString(", ")} dichiarando ${play.declared}")
      case _: Engine.GameCommand.CallBluff =>
        println("🤖 Bot chiama BLUFF!")
      case _ =>
        println("🤖 Bot esegue un comando imprevisto")

    // call engine and append a BotPlayed event when appropriate so the UI sees it
    Engine.step(state, move) match
      case Left(err) => Left(err)
      case Right((st2, evs)) =>
        move match
          case play: Engine.GameCommand.Play => Right((st2, evs :+ Engine.GameEvent.BotPlayed(play.player, play.declared, play.cards.size)))
          case _ => Right((st2, evs))
