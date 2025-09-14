package it.unibo.bluff.model.bot

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.PlayerId
import it.unibo.bluff.model.core.engine.Engine

/** Controlla periodicamente lo stato e, se è il turno del bot, gioca una mossa. */
final class BotRunner(
                       stateRef: AtomicReference[GameState],
                       bot: RandomBot,
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
        case Left(_) => () // nessuna mossa valida, ignora
        case Right((newSt, evs)) =>
          // 1) aggiorna lo stato condiviso
          stateRef.set(newSt)
          // 2) notifica GUI / controller
          BotManager.onEvents(evs)
          onNewState(newSt)

          // 3) controlla se c'è GameEnded tra gli eventi
          if evs.exists {
            case it.unibo.bluff.model.core.engine.Engine.GameEvent.GameEnded(_) => true
            case _ => false
          } then
            onGameEnded()

          // 4) aggiorna ultimo turno
          lastTurn = Some(st.turn)
      }
    else {
      // se non è turno del bot, aggiorna solo il marcatore
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
