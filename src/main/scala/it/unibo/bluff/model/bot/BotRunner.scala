package it.unibo.bluff.model.bot

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.PlayerId

/** Controlla periodicamente lo stato e, se è il turno del bot, gioca una mossa. */
class BotRunner(
  stateRef: AtomicReference[GameState],
  bot: RandomBot,
  pollMillis: Long = 300L,
  onNewState: GameState => Unit = _ => (),                           // ⬅️ callback per sincronizzare il Controller
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
            case Left(_) => ()
            case Right((newSt, evs)) =>
              // 1) sincronizza Controller/altro stato applicativo
              onNewState(newSt)
              // 2) aggiorna il riferimento condiviso
              stateRef.set(newSt)
              // 3) notifica la GUI (GameView è sottoscritta)
              BotManager.onEvents(evs)
              // 4) segna il turno processato
              lastTurn = Some(newSt.turn)
          }
        } else {
          // avanza il "marcatore" per evitare doppi turni
          lastTurn = Some(st.turn)
        }
      } catch { case _: Throwable => () }
  }

  def start(): Unit =
    if (running.compareAndSet(false, true))
      scheduler.scheduleAtFixedRate(task, 0, pollMillis, TimeUnit.MILLISECONDS)

  def stop(): Unit =
    if (running.compareAndSet(true, false))
      scheduler.shutdownNow()
}
