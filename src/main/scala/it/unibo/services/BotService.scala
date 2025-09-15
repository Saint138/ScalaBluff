package it.unibo.bluff.services

import it.unibo.bluff.interfaces.*
import it.unibo.bluff.model.*
import it.unibo.bluff.model.bot.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Servizio per la gestione dei bot
 * Completamente disaccoppiato dalla UI
 */
class BotService(controller: IGameController) extends IBotService:
  
  private var currentBot: Option[Bot] = None
  private val scheduler: ScheduledExecutorService = 
    Executors.newSingleThreadScheduledExecutor()
  private val running = AtomicBoolean(false)
  
  def startBot(playerId: PlayerId, botType: String): Unit =
    stopBot()
    currentBot = Some(BotFactory(botType, playerId))
    running.set(true)
    
    // Schedula l'esecuzione periodica del bot
    scheduler.scheduleAtFixedRate(
      () => checkAndExecuteBotTurn(),
      0, 300, TimeUnit.MILLISECONDS
    )
  
  def stopBot(): Unit =
    running.set(false)
    currentBot = None
  
  def isBotRunning: Boolean = running.get()
  
  def executeBotTurn(state: GameState): Either[String, List[GameEvent]] =
    currentBot match
      case None => 
        Left("Nessun bot attivo")
      case Some(bot) if state.turn != bot.id =>
        Left("Non è il turno del bot")
      case Some(bot) =>
        val command = bot.decideMove(state)
        controller.handleCommand(command)
  
  private def checkAndExecuteBotTurn(): Unit =
    if running.get() then
      controller.currentState.foreach { state =>
        currentBot.foreach { bot =>
          if state.turn == bot.id then
            executeBotTurn(state)
        }
      }
  
  def shutdown(): Unit =
    stopBot()
    scheduler.shutdownNow()