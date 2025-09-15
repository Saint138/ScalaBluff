package it.unibo.bluff.coordinator

import it.unibo.bluff.interfaces.*
import it.unibo.bluff.controller.ImprovedGameController
import it.unibo.bluff.services.BotService
import it.unibo.bluff.view.gui.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.stats.MatchStats
import it.unibo.bluff.model.PlayerId
import scalafx.application.Platform
import scalafx.scene.layout.BorderPane
import scalafx.stage.Stage
import scalafx.scene.control.Alert
import scalafx.Includes.*                             
import it.unibo.bluff.model.core.engine.Engine.GameEvent  


/**
 * Coordinatore principale che orchestra MVC
 * Questa è l'unica classe che conosce tutti i componenti
 */
class GameCoordinator:
  
  private val controller = new ImprovedGameController()
  private val botService = new BotService(controller)
  private var currentView: Option[ImprovedGameView] = None
  private var currentStage: Option[Stage] = None
  
  
  // Configurazione partita
  private var tournamentRounds = 1
  private var currentRound = 1
  private var playerNames = Vector.empty[String]
  private var isVsBot = false
  private var botType = "random"
  
  /**
   * Avvia una nuova partita multiplayer
   */
  def startMultiplayer(names: Vector[String], rounds: Int, stage: Stage): Unit =
    cleanup()
    
    playerNames = names
    tournamentRounds = rounds
    currentRound = 1
    isVsBot = false
    currentStage = Some(stage)
    
    startNewRound(stage)
  
  /**
   * Avvia una partita contro il bot
   */
  def startVsBot(humanName: String, rounds: Int, stage: Stage, botKind: String): Unit =
    cleanup()
    
    playerNames = Vector(humanName, "Bot")
    tournamentRounds = rounds
    currentRound = 1
    isVsBot = true
    botType = botKind
    currentStage = Some(stage)
    
    startNewRound(stage)
  
  /**
   * Mostra il dialog per una nuova partita
   */
  def showNewGameDialog(stage: Stage): Unit =
    NewGameDialog.askPlayers().foreach { case (isSingle, names, rounds, chosenBotKind) =>
      if isSingle then 
        startVsBot(names.head, rounds, stage, chosenBotKind)
      else 
        startMultiplayer(names, rounds, stage)
    }
  
  /**
   * Mostra le regole del gioco
   */
  def showRules(stage: Stage): Unit =
    RulesDialog.show(() => showMainMenu(stage))
  
  /**
   * Torna al menu principale
   */
  def showMainMenu(stage: Stage): Unit =
    cleanup()
    Platform.runLater {
      stage.scene().root = MainMenuView(
        onNewGame = () => showNewGameDialog(stage),
        onRules = () => showRules(stage)
      )
    }
  
  /**
   * Cleanup delle risorse
   */
  def shutdown(): Unit =
    cleanup()
    botService.shutdown()
  
  // === Metodi privati ===
  
  private def startNewRound(stage: Stage): Unit =
    // Inizializza il controller per il nuovo round
    controller.startNewGame(playerNames, tournamentRounds)
    
    // Se è contro bot, avvia il servizio bot
    if isVsBot then
      val botId = PlayerId(1) // Assumendo che il bot sia sempre il player 1
      botService.startBot(botId, botType)
    
    // Crea e mostra la vista di gioco
    Platform.runLater {
      val gameView = new ImprovedGameView(
        controller = controller,
        onExitToMenu = () => showMainMenu(stage)
      )
      
      // Aggiungi un observer per gestire fine round/torneo
      controller.subscribe(new GameObserver {
        override def onStateChanged(state: GameState): Unit = ()
        override def onEvents(events: List[GameEvent]): Unit = ()
        
        override def onRoundEnd(state: GameState, stats: MatchStats, hasMoreRounds: Boolean): Unit = 
          Platform.runLater {
            if currentRound < tournamentRounds then
              currentRound += 1
              new Alert(Alert.AlertType.Information) {
                headerText = "Round concluso"
                contentText = s"Preparazione round $currentRound di $tournamentRounds"
              }.showAndWait()
              startNewRound(stage)
            else
              onTournamentEnd(state, stats)
          }
        
        override def onTournamentEnd(state: GameState, stats: MatchStats): Unit =
          Platform.runLater {
            import gui.StatsDialog
            StatsDialog.show("Torneo concluso", "Statistiche finali del torneo")
            showMainMenu(stage)
          }
      })
      
      currentView = Some(gameView)
      stage.scene().root = new BorderPane { center = gameView }
    }
  
  private def cleanup(): Unit =
    currentView.foreach(v => controller.unsubscribe(v))
    currentView = None
    botService.stopBot()

// Singleton per accesso globale (opzionale)
object GameCoordinator:
  private lazy val instance = new GameCoordinator()
  
  def apply(): GameCoordinator = instance
  
  def startMultiplayer(names: Vector[String], rounds: Int, stage: Stage): Unit =
    instance.startMultiplayer(names, rounds, stage)
  
  def startVsBot(humanName: String, rounds: Int, stage: Stage, botKind: String): Unit =
    instance.startVsBot(humanName, rounds, stage, botKind)
  
  def showNewGameDialog(stage: Stage): Unit =
    instance.showNewGameDialog(stage)
  
  def showRules(stage: Stage): Unit =
    instance.showRules(stage)
  
  def shutdown(): Unit =
    instance.shutdown()