package it.unibo.bluff.model.bot

import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.GameEvent
import it.unibo.bluff.model.core.state.GameState

object BotManager:

  /** Callback opzionale usata dalla GUI; lascia com'è se già presente altrove */
  @volatile var onEvents: List[Engine.GameEvent] => Unit = _ => ()
  /** Esegue il turno del bot */
  def takeTurn(bot: RandomBot, state: GameState): Either[String, (GameState, List[GameEvent])] =
    
    val move = bot.decideMove(state)

    println(s"[DEBUG] Prima di step: turno=${state.turn}, bot=${bot.id}")
    val result = Engine.step(state, move)
    result.foreach { case (st2, evs) =>
      println(s"[DEBUG] Dopo step: turno=${st2.turn}, eventi=$evs")
    }
    

    move match
      case play: Engine.GameCommand.Play =>
        println(s"🤖 Bot gioca: ${play.cards.map(_.rank).mkString(", ")} dichiarando ${play.declared}")
      case _: Engine.GameCommand.CallBluff =>
        println("🤖 Bot chiama BLUFF!")
      case _ =>
        println("🤖 Bot esegue un comando imprevisto")
    Engine.step(state, move) match
      case Left(err) => Left(err)
      case Right((st2, evs)) =>
        move match
          case play: Engine.GameCommand.Play =>
            val evsNoPlayed = evs.filter {
              case Engine.GameEvent.Played(p, _, _) if p == play.player => false
              case _                                                    => true
            }
            val botEv = Engine.GameEvent.BotPlayed(play.player, play.declared, play.cards.size)
            Right((st2, evsNoPlayed :+ botEv))
          case _ =>
            Right((st2, evs))
