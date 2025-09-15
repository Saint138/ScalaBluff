package it.unibo.bluff.view.gui

import it.unibo.bluff.interfaces.*
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.stats.MatchStats
import it.unibo.bluff.model.cards.Card
import it.unibo.bluff.view.gui.components.*
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scala.collection.mutable

/**
 * Vista di gioco che implementa GameObserver
 * Non ha dipendenze dirette da controller concreti o AtomicReference
 */
class ImprovedGameView(
  controller: IGameController,
  onExitToMenu: () => Unit = () => ()
) extends BorderPane with GameObserver {
  
  // Stato locale della vista
  private var currentState: Option[GameState] = None
  private val selected = mutable.LinkedHashSet.empty[Card]
  private val handNodes = mutable.Buffer.empty[CardNode.CardNode]
  
  // Componenti UI
  private val header = HeaderBar()
  private val handPane = HandPanel()
  private val logArea = LogPanel()
  private val actions = ActionsPanel()
  
  // Bottone per terminare la partita
  private val btnEnd = new Button("Termina partita") {
    style = "-fx-background-color:#ef5350; -fx-text-fill:white; -fx-font-weight:bold;"
    onAction = _ => handleEndGame()
  }
  
  private def handleEndGame(): Unit = {
    val res = new Alert(Alert.AlertType.Confirmation) {
      title = "Termina partita"
      headerText = "Vuoi davvero terminare la partita?"
      contentText = "Perderai i progressi della partita in corso."
      buttonTypes = Seq(ButtonType.Cancel, ButtonType.OK)
    }.showAndWait()
    if res.exists(_ == ButtonType.OK) then 
      controller.unsubscribe(ImprovedGameView.this)
      onExitToMenu()
  }
  
  // Layout
  top = new BorderPane {
    left = header
    right = new HBox { 
      spacing = 8
      padding = Insets(8)
      children = Seq(btnEnd)
    }
  }
  
  center = new HBox(16,
    new VBox(12, handPane) { padding = Insets(10) },
    new VBox(8, actions, logArea) { padding = Insets(10) }
  ) { 
    padding = Insets(10, 16, 10, 16)
  }
  
  // Registra questa vista come osservatore
  controller.subscribe(this)
  
  // === Implementazione GameObserver ===
  
  override def onStateChanged(state: GameState): Unit = Platform.runLater {
    currentState = Some(state)
    updateUI()
  }
  
  override def onEvents(events: List[GameEvent]): Unit = Platform.runLater {
    currentState.foreach { state =>
      events.foreach { event =>
        val messages = controller.renderEvent(event, state)
        messages.foreach(msg => logArea.appendText(msg + "\n"))
      }
    }
  }
  
  override def onStatsUpdated(stats: MatchStats): Unit = Platform.runLater {
    // Potrebbe aggiornare un pannello statistiche se presente
  }
  
  override def onRoundEnd(state: GameState, stats: MatchStats, hasMoreRounds: Boolean): Unit = 
    Platform.runLater {
      import gui.StatsDialog
      import it.unibo.bluff.model.stats.StatsUpdater
      StatsDialog.show("Round concluso", StatsUpdater.pretty(state, stats))
      
      if !hasMoreRounds then
        controller.unsubscribe(this)
        onExitToMenu()
    }
  
  // === Metodi privati ===
  
  private def updateUI(): Unit = currentState.foreach { state =>
    updateHeader(state)
    renderHand(state)
    updateButtonsEnabled(state)
  }
  
  private def updateHeader(state: GameState): Unit = {
    val matchStart = System.currentTimeMillis() // Potresti gestirlo meglio
    header.update(state, matchStart, 60000L)
  }
  
  private def renderHand(state: GameState): Unit = {
    handPane.children.clear()
    handNodes.clear()
    
    val cards = state.hands.getOrElse(state.turn, Hand.empty).cards
      .sortBy(c => (c.rank.ordinal, c.suit.ordinal))
    
    cards.foreach { c =>
      val node = CardNode(c, toggleSelect)
      handNodes += node
      handPane.children.add(node)
    }
  }
  
  private def toggleSelect(n: CardNode.CardNode): Unit = {
    val c = n.card
    if selected.contains(c) then
      selected.remove(c)
      n.markSelected(false)
    else
      selected.add(c)
      n.markSelected(true)
    
    currentState.foreach(updateButtonsEnabled)
  }
  
  private def updateButtonsEnabled(state: GameState): Unit = {
    actions.updateButtons(state, selected.nonEmpty)
  }
  
  private def resetSelection(): Unit = {
    selected.clear()
    handNodes.foreach(_.markSelected(false))
    currentState.foreach(updateButtonsEnabled)
  }
  
  // === Azioni ===
  
  actions.onPlay { declaredRank =>
    currentState.foreach { state =>
      val toPlay = selected.toList
      if toPlay.nonEmpty && declaredRank != null then
        val result = controller.handleCommand(
          GameCommand.Play(state.turn, toPlay, declaredRank)
        )
        result match
          case Left(error) =>
            new Alert(Alert.AlertType.Error) {
              headerText = "Mossa non valida"
              contentText = error
            }.showAndWait()
          case Right(_) =>
            resetSelection()
    }
  }
  
  actions.onCall {
    currentState.foreach { state =>
      controller.handleCommand(GameCommand.CallBluff(state.turn))
    }
  }
  
  actions.onClear {
    resetSelection()
  }
}