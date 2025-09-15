package it.unibo.bluff.controller

import it.unibo.bluff.interfaces.*
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.stats.{MatchStats, StatsUpdater}
import it.unibo.bluff.model.TurnOrder.given
import scala.collection.mutable

/** 
 * Controller MVC migliorato con Observer Pattern
 * Mantiene lo stato, gestisce la logica e notifica gli osservatori
 */
class ImprovedGameController extends IGameController:
  
  private var state: Option[GameState] = None
  private var stats: Option[MatchStats] = None
  private val observers = mutable.ListBuffer[GameObserver]()
  
  // Public API
  def currentState: Option[GameState] = state
  def currentStats: Option[MatchStats] = stats
  
  def subscribe(observer: GameObserver): Unit = 
    observers += observer
    // Notifica lo stato corrente al nuovo osservatore
    state.foreach(observer.onStateChanged)
    stats.foreach(observer.onStatsUpdated)
  
  def unsubscribe(observer: GameObserver): Unit = 
    observers -= observer
  
  def startNewGame(players: Vector[String], rounds: Int): Unit =
    // Inizializza nuovo gioco
    val (st, evs, _) = it.unibo.bluff.model.core.setup.GameSetup.fairInitialDeal(
      players.size, 
      players
    )
    setGameState(st)
    notifyEvents(evs)
  
  def handleCommand(cmd: GameCommand): Either[String, List[GameEvent]] =
    state match
      case None => 
        Left("Nessuna partita in corso")
      case Some(st) =>
        val prevState = st
        Engine.step(st, cmd) match
          case Left(error) => 
            Left(error)
          case Right((newState, events)) =>
            // Aggiorna stato
            state = Some(newState)
            
            // Aggiorna statistiche
            stats = stats.map(ms => 
              StatsUpdater(prevState, events, newState, ms)
            )
            
            // Notifica tutti gli osservatori
            notifyStateChanged(newState)
            notifyEvents(events)
            stats.foreach(notifyStatsUpdated)
            
            // Controlla fine partita
            events.foreach {
              case GameEvent.GameEnded(winner) =>
                notifyGameEnded(newState, winner)
              case _ => ()
            }
            
            Right(events)
  
  def renderEvent(ev: GameEvent, st: GameState): List[String] = ev match
    case GameEvent.Dealt(sz) =>
      sz.map { case (p, s) => s"Distribuite carte: ${st.nameOf(p)}=$s" }.toList
    case GameEvent.Played(p, d, c) =>
      List(s"${st.nameOf(p)} dichiara $d e gioca $c carte")
    case GameEvent.BotPlayed(p, d, c) =>
      List(s"(BOT) ${st.nameOf(p)} dichiara $d e gioca $c carte")
    case GameEvent.BluffCalled(by, ag, truth) =>
      List(s"${st.nameOf(by)} accusa ${st.nameOf(ag.player)} → " + 
           (if truth then "VERA" else "FALSA"))
    case GameEvent.TimerExpired(p) =>
      List(s"Timeout: ${st.nameOf(p)} ha esaurito il tempo.")
    case GameEvent.QuartetCleared(p, r, cnt) =>
      List(s"♻️ ${st.nameOf(p)} elimina automaticamente $cnt carte ($r)")
    case GameEvent.GameEnded(w) =>
      List(s"🏆 Vince ${st.nameOf(w)}!")
    case _ => Nil
  
  // Private methods
  private def setGameState(st: GameState): Unit =
    state = Some(st)
    stats = Some(MatchStats.empty(st.players))
    notifyStateChanged(st)
  
  private def notifyStateChanged(state: GameState): Unit =
    observers.foreach(_.onStateChanged(state))
  
  private def notifyEvents(events: List[GameEvent]): Unit =
    observers.foreach(_.onEvents(events))
  
  private def notifyStatsUpdated(stats: MatchStats): Unit =
    observers.foreach(_.onStatsUpdated(stats))
  
  private def notifyGameEnded(state: GameState, winner: PlayerId): Unit =
  stats.foreach { s =>
    // Non passare hasMoreRounds qui. Lascia decidere al Coordinator.
    observers.foreach(_.onRoundEnd(state, s, hasMoreRounds = true)) // view non deve uscire
  }
