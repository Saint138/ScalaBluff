package it.unibo.bluff.model.bot

import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.controller.GameController

/** BotManager centralizza l’esecuzione dei bot e l’aggiornamento degli eventi/stats */
object BotManager:

  
  /** Callback opzionale: GUI o altri componenti possono aggiornare la UI */
  @volatile var onEvents: List[GameEvent] => Unit = _ => ()
  /** Callback opzionale: aggiornamento stato globale (es. AtomicReference) */
  @volatile var onStateUpdate: GameState => Unit = _ => ()

  /** Callback opzionale: esegue il comando del bot tramite il controller */
  @volatile var executeCommand: GameCommand => Either[String, (GameState, List[GameEvent])] = _ => Right((null, Nil))
  

  /** Esegue il turno del bot */
  def takeTurn(bot: Bot, state: GameState): Either[String, (GameState, List[GameEvent])] =
    val move   = bot.decideMove(state)
    val result = executeCommand(move)

    result match
      case Left(err) => Left(err)
      case Right((st2, evs)) =>
        // 🔄 Aggiorna lo stato globale
        onStateUpdate(st2)
        // 🔄 Aggiorna le statistiche tramite il controller, se presente

        // Debug
        println(s"[DEBUG] Bot=${bot.id}, Move=$move, Turno successivo=${st2.turn}, Eventi=$evs")

        // Riconversione Play -> BotPlayed per logging
        val finalEvents = move match
          case play: GameCommand.Play =>
            val evsNoPlayed = evs.filter {
              case GameEvent.Played(p, _, _) if p == play.player => false
              case _                                             => true
            }
            val botEv = GameEvent.BotPlayed(play.player, play.declared, play.cards.size)

            evsNoPlayed.flatMap {
              case ge @ GameEvent.GameEnded(_) => Seq(botEv, ge)
              case other                       => Seq(other)
            } match
              case seq if !seq.contains(botEv) => seq :+ botEv
              case seq                         => seq

          case _ => evs

        // 🔄 Notifica eventi alla GUI
        onEvents(finalEvents)
        Right((st2, finalEvents))
