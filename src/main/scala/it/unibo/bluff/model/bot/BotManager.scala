package it.unibo.bluff.model.bot

import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.core.state.GameState

object BotManager:

  /** Callback opzionale: la GUI (o il Controller) la setta per aggiornare lo stato */
  @volatile var onEvents: List[GameEvent] => Unit = _ => ()
  /** Callback opzionale: per aggiornare lo stato globale (es. stateRef.set) */
  @volatile var onStateUpdate: GameState => Unit = _ => ()

  /** Esegue il turno del bot */
  def takeTurn(bot: RandomBot, state: GameState): Either[String, (GameState, List[GameEvent])] =
    val move   = bot.decideMove(state)
    val result = Engine.step(state, move)

    result match
      case Left(err) => Left(err)

      case Right((st2, evs)) =>
        // 🔄 Aggiorna lo stato globale (per checkRoundEnd e statistiche)
        onStateUpdate(st2)

        // Debug console
        println(s"[DEBUG] Bot=${bot.id}, Move=$move, Turno successivo=${st2.turn}, Eventi=$evs")

        move match
          case play: GameCommand.Play =>
            println(s"🤖 Bot gioca: ${play.cards.map(_.rank).mkString(", ")} dichiarando ${play.declared}")

            // Rimuove Played originale e sostituisce con BotPlayed, mantenendo l'ordine rispetto a GameEnded
            val evsNoPlayed = evs.filter {
              case GameEvent.Played(p, _, _) if p == play.player => false
              case _                                             => true
            }
            val botEv = GameEvent.BotPlayed(play.player, play.declared, play.cards.size)

            val finalEvs =
              evsNoPlayed.flatMap {
                case ge @ GameEvent.GameEnded(_) => Seq(botEv, ge) // prima BotPlayed, poi GameEnded
                case other                        => Seq(other)
              } match
                case seq if !seq.contains(botEv) => seq :+ botEv
                case seq                         => seq

            onEvents(finalEvs)
            Right((st2, finalEvs))

          case _ =>
            // CallBluff o altri comandi
            println(s"🤖 Bot esegue: $move")
            onEvents(evs)
            Right((st2, evs))
