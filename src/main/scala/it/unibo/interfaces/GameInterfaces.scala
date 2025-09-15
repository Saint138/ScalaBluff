package it.unibo.bluff.interfaces

import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.stats.MatchStats

/** Interfaccia per osservare i cambiamenti del gioco */
trait GameObserver:
  def onStateChanged(state: GameState): Unit
  def onEvents(events: List[GameEvent]): Unit
  def onStatsUpdated(stats: MatchStats): Unit = () // default implementation
  def onRoundEnd(state: GameState, stats: MatchStats, hasMoreRounds: Boolean): Unit = ()
  def onTournamentEnd(state: GameState, stats: MatchStats): Unit = ()

/** Interfaccia principale del Controller */
trait IGameController:
  def handleCommand(cmd: GameCommand): Either[String, List[GameEvent]]
  def currentState: Option[GameState]
  def currentStats: Option[MatchStats]
  def subscribe(observer: GameObserver): Unit
  def unsubscribe(observer: GameObserver): Unit
  def startNewGame(players: Vector[String], rounds: Int): Unit
  def renderEvent(ev: GameEvent, st: GameState): List[String]

/** Interfaccia per la gestione dei round */
trait IRoundManager:
  def initTournament(names: Vector[String], rounds: Int): Unit
  def startRound(): GameState
  def checkRoundEnd(): Unit
  def currentRound: Int
  def totalRounds: Int
  def isLastRound: Boolean = currentRound >= totalRounds

/** Interfaccia per la gestione dei bot */
trait IBotService:
  def startBot(playerId: PlayerId, botType: String): Unit
  def stopBot(): Unit
  def isBotRunning: Boolean
  def executeBotTurn(state: GameState): Either[String, List[GameEvent]]

/** Interfaccia per il timer di gioco */
trait IGameTimer:
  def start(): Unit
  def stop(): Unit
  def pause(): Unit
  def resume(): Unit
  def isRunning: Boolean