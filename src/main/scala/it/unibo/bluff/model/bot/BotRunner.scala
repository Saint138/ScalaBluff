package it.unibo.bluff.model.bot

import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.PlayerId

/** BotRunner: periodically checks the shared stateRef and, when it's the bot's turn,
  * uses BotManager to decide and apply a move, updating the shared stateRef.
  */
class BotRunner(stateRef: AtomicReference[GameState], bot: RandomBot, pollMillis: Long = 300L,
                scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(),
                logCb: Option[String => Unit] = None) {
  private val running = new AtomicBoolean(false)
  private var lastTurn: Option[PlayerId] = None

  private val task = new Runnable {
    override def run(): Unit =
      try {
        val st = stateRef.get()
        if (st.turn == bot.id && lastTurn.forall(_ != bot.id)) {
          BotManager.takeTurn(bot, st) match {
            case Left(err) =>
              val s = s"Bot error: $err"
              logCb.foreach(cb => cb(s)); if logCb.isEmpty then println(s)
            case Right((newSt, evs)) =>
              stateRef.set(newSt)
              evs.foreach { ev =>
                val s = s"Bot event: $ev"
                logCb.foreach(cb => cb(s))
                if logCb.isEmpty then println(s)
              }
          }
        }
        lastTurn = Some(st.turn)
      } catch {
        case t: Throwable =>
          val s = s"BotRunner error: ${t.getMessage}"
          logCb.foreach(cb => cb(s)); if logCb.isEmpty then t.printStackTrace()
      }
  }

  def start(): Unit =
    if running.compareAndSet(false, true) then
      scheduler.scheduleAtFixedRate(task, 0, pollMillis, TimeUnit.MILLISECONDS)

  def stop(): Unit =
    if running.compareAndSet(true, false) then
      scheduler.shutdownNow()
}
