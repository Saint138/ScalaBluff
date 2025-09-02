package it.unibo.bluff.timer

import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.state.{GameState, GameClocks}
import it.unibo.bluff.model.PlayerId

/**
 * GameTimer: semplice scheduler che decrementa il clock del giocatore corrente
 * e chiama la callback `onTimeout` una volta quando il clock scade.
 *
 * - Non esegue la logica di gioco; si limita a modificare lo stato (tick) e
 *   notifica il caller quando occorre forzare un Timeout.
 */
class GameTimer(
  stateRef: AtomicReference[GameState],
  tickMillis: Long = 200L,
  perTurnMillis: Long = 60000L,
  scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(),
  onTimeout: PlayerId => Unit // callback eseguita quando un player's clock scade
) {
  private val running = new AtomicBoolean(false)
  // manteniamo l'ultimo valore rimasto per ciascun giocatore per evitare ripetute notifiche
  private var lastRemaining: Map[PlayerId, Long] = Map.empty
  // ricordiamo l'ultimo giocatore di cui abbiamo visto il turno per resettare il clock al cambio di turno
  private var lastTurn: Option[PlayerId] = None

  private val task = new Runnable {
    override def run(): Unit =
      try {
        val old = stateRef.get()
        val current = old.turn
        val withReset = lastTurn match
          case Some(prev) if prev == current => old
          case _ =>
            val r = GameClocks.setClock(old, current, perTurnMillis)
            r
        lastTurn = Some(current)
        val prevRem = withReset.clocks.getOrElse(current, 0L)
        val ticked = if (prevRem > 0L) GameClocks.tickClock(withReset, current, tickMillis) else withReset
        stateRef.set(ticked)
        val afterRem = ticked.clocks.getOrElse(current, 0L)

        val last = lastRemaining.getOrElse(current, Long.MaxValue)
        if (afterRem <= 0L && last > 0L) {
          onTimeout(current)
        }
        lastRemaining = lastRemaining.updated(current, afterRem)
      } catch {
        case t: Throwable => t.printStackTrace()
      }
  }

  def start(): Unit =
    if running.compareAndSet(false, true) then
      scheduler.scheduleAtFixedRate(task, 0, tickMillis, TimeUnit.MILLISECONDS)

  def stop(): Unit =
    if running.compareAndSet(true, false) then
      scheduler.shutdownNow()
}
