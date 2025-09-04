package it.unibo.bluff.model.bot

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.model.PlayerId

/** BotRunner: controlla periodicamente lo stato e, se è il turno del bot, gioca una mossa. */
class BotRunner(
  stateRef: AtomicReference[GameState],
  bot: RandomBot,
  pollMillis: Long = 300L,
  scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
) {
  private val running = new AtomicBoolean(false)
  private var lastTurn: Option[PlayerId] = None

  private val task = new Runnable {
    override def run(): Unit =
      try {
        val st = stateRef.get()
        if (st.turn == bot.id && lastTurn.forall(_ != bot.id)) {
          BotManager.takeTurn(bot, st) match {
            case Left(_) =>
              () // niente println qui: il log è gestito esclusivamente dalla GameView
            case Right((newSt, evs)) =>
              stateRef.set(newSt)
              // inoltra gli eventi alla GUI (GameView è iscritta tramite BotManager.onEvents)
              BotManager.onEvents(evs)
          }
        }
        lastTurn = Some(st.turn)
      } catch { case _: Throwable => () }
  }

  def start(): Unit =
    if (running.compareAndSet(false, true))
      scheduler.scheduleAtFixedRate(task, 0, pollMillis, TimeUnit.MILLISECONDS)

  def stop(): Unit =
    if (running.compareAndSet(true, false))
      scheduler.shutdownNow()
}
