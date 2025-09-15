package it.unibo.bluff.model.bot

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.PlayerId
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.GameEvent

final class BotRunner(
                       stateRef: AtomicReference[GameState],
                       bot: Bot,
                       pollMillis: Long = 300L,
                       onNewState: GameState => Unit = _ => (),
                       onGameEnded: () => Unit = () => (),
                       scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
                     ) {

  @volatile private var running: Boolean = false
  private var lastTurn: Option[PlayerId] = None

  /** Task eseguito periodicamente */
  private def task(): Unit = {
    val st = stateRef.get()
    if st.turn == bot.id then
      BotManager.takeTurn(bot, st) match {
        case Left(_) => () 
        case Right((newSt, evs)) =>
          stateRef.set(newSt)
          onNewState(newSt)
          if evs.exists {
            case GameEvent.GameEnded(_) => true
            case _ => false
          } then
            onGameEnded()
          lastTurn = Some(st.turn)
      }
    else {
      lastTurn = Some(st.turn)
    }
  }

  /** Avvia il polling del bot */
  def start(): Unit =
    if !running then
      running = true
      scheduler.scheduleAtFixedRate(() => task(), 0, pollMillis, TimeUnit.MILLISECONDS)

  /** Ferma il bot e lo scheduler */
  def stop(): Unit =
    if running then
      running = false
      scheduler.shutdownNow()
}
