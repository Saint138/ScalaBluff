package it.unibo.bluff.view.gui

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.model.core.engine.Engine
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.view.gui.components.*
import it.unibo.bluff.model.cards.Card
import scalafx.Includes.*
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.*
import scalafx.scene.effect.GaussianBlur
import scalafx.scene.layout.*
import scalafx.util.Duration

import scala.collection.mutable

object GameView {

  def apply(
    stateRef: AtomicReference[GameState],
    maxPerTurnMs: Long = 60_000L,
    dispatch: GameCommand => Either[String, List[GameEvent]],
    renderEvent: (GameEvent, GameState) => List[String],
    subscribeToExternalEvents: (List[GameEvent] => Unit) => Unit = _ => (),
    onGameEnded: PlayerId => Unit = _ => (),
    onExitToMenu: () => Unit = () => (),
    onOverlayChange: Boolean => Unit = _ => () // true=overlay ON, false=OFF
  ): BorderPane =
    new BorderPane {

      private def st: GameState = stateRef.get()

      private val selected  = mutable.LinkedHashSet.empty[Card]
      private val handNodes = mutable.Buffer.empty[CardNode.CardNode]

      private def clearSelectionVisual(): Unit =
        handNodes.foreach(_.markSelected(false))

      private def resetSelection(): Unit =
        selected.clear()
        clearSelectionVisual()
        updateButtonsEnabled()

      private val header    = HeaderBar()
      private val handPane  = HandPanel()
      private val logArea   = LogPanel()
      private val actions   = ActionsPanel()

      actions.style = "-fx-font-size: 14px;"

      private var currentViewer: PlayerId = st.turn
      private var overlayShown: Boolean = false
      private var gameEnded: Boolean = false

      private val centerContent = new HBox(16,
        new VBox(12, handPane) { padding = Insets(10) },
        new VBox(8, actions, logArea) { padding = Insets(10) }
      ) { padding = Insets(10, 16, 10, 16) }

      private val overlayLabel = new Label {
        style = "-fx-text-fill: white; -fx-font-size: 18px; -fx-font-weight: bold;"
      }
      private val btnReady = new Button("Sono pronto") {
        style = "-fx-font-size:16px; -fx-font-weight:bold; -fx-padding:10 18 10 18;"
        minWidth = 200
        defaultButton = true
        onAction = _ => {
          hideOverlay()
          currentViewer = st.turn
        }
      }
      private val overlayPane = new VBox {
        alignment = Pos.Center
        spacing = 18
        padding = Insets(28)
        style = "-fx-background-color: rgba(0,0,0,0.60);"
        visible = false
        pickOnBounds = true
        children = Seq(
          new Label("Passa il dispositivo al prossimo giocatore") {
            style = "-fx-text-fill: white; -fx-font-size: 16px;"
          },
          overlayLabel,
          btnReady
        )
      }
      StackPane.setAlignment(overlayPane, Pos.Center)

      private def shouldUseOverlay: Boolean =
        st.players.size == 2 && !st.players.exists(pid => st.nameOf(pid).equalsIgnoreCase("bot"))

      private def showOverlay(next: PlayerId): Unit = {
        overlayLabel.text = s"Sarà il turno di ${st.nameOf(next)}"
        centerContent.effect = new GaussianBlur(16)
        overlayPane.visible = true
        overlayPane.toFront()
        actions.disable = true
        handPane.visible = false
        overlayShown = true
        onOverlayChange(true)
      }

      private def hideOverlay(): Unit = {
        overlayPane.visible = false
        centerContent.effect = null
        actions.disable = false
        handPane.visible = true
        overlayShown = false
        onOverlayChange(false)
      }

      private def maybeShowOverlayOnTurnChange(): Unit = {
        if !gameEnded && shouldUseOverlay then
          val next = st.turn
          if !overlayShown && next != currentViewer then
            showOverlay(next)
      }

      private val btnEnd = new Button("Termina partita") {
        style = "-fx-background-color:#ef5350; -fx-text-fill:white; -fx-font-weight:bold;"
        onAction = _ => {
          val res = new Alert(Alert.AlertType.Confirmation) {
            title = "Termina partita"
            headerText = "Vuoi davvero terminare la partita?"
            contentText = "Perderai i progressi della partita in corso."
            buttonTypes = Seq(ButtonType.Cancel, ButtonType.OK)
          }.showAndWait()
          if res.exists(_ == ButtonType.OK) then onExitToMenu()
        }
      }

      top = new BorderPane {
        left  = header
        right = new HBox { spacing = 8; padding = Insets(8); children = Seq(btnEnd) }
      }

      center = new StackPane {
        children = Seq(centerContent, overlayPane)
      }

      private def toggleSelect(n: CardNode.CardNode): Unit =
        val c = n.card
        if selected.contains(c) then
          selected.remove(c)
          n.markSelected(false)
        else
          selected.add(c)     
          n.markSelected(true)
        updateButtonsEnabled()

      private def renderHand(): Unit =
        handPane.children.clear()
        handNodes.clear()
        val cards = st.hands.getOrElse(st.turn, Hand.empty).cards
          .sortBy(c => (c.rank.ordinal, c.suit.ordinal))
        cards.foreach { c =>
          val node = CardNode(c, toggleSelect)
          handNodes += node
          handPane.children.add(node)
        }
        updateButtonsEnabled()

      private val matchStart = System.currentTimeMillis()

      private def updateHeader(): Unit =
        header.update(st, matchStart, maxPerTurnMs)

      private def updateButtonsEnabled(): Unit =
        actions.updateButtons(st, selected.nonEmpty)

      private def updateAll(): Unit = {
        updateHeader()
        renderHand()
        maybeShowOverlayOnTurnChange()
      }

      private def appendEvent(ev: Engine.GameEvent): Unit =
        val messages = renderEvent(ev, st)
        messages.foreach(msg => logArea.appendText(msg + "\n"))
        if ev.isInstanceOf[Engine.GameEvent.GameEnded] then
          uiTick.stop()
          ev match
            case Engine.GameEvent.GameEnded(w) => onGameEnded(w)
            case _ => ()

      private def send(cmd: GameCommand): Unit =
        val result = dispatch(cmd)
        result match
          case Left(err) =>
            new Alert(Alert.AlertType.Error) {
              headerText = "Mossa non valida"
              contentText = err
            }.showAndWait()
          case Right(events) =>
            events.foreach(appendEvent)
            updateAll()

      actions.onPlay { decl =>
        val toPlay = selected.toList
        if toPlay.nonEmpty && decl != null then
          send(GameCommand.Play(st.turn, toPlay, decl))
          resetSelection()
      }
      actions.onCall { send(GameCommand.CallBluff(st.turn)) }
      actions.onClear { resetSelection() }

      private val uiTick = Timeline(KeyFrame(Duration(200), onFinished = _ => updateHeader()))
      uiTick.cycleCount = Timeline.Indefinite
      uiTick.play()

      subscribeToExternalEvents { evs =>
        Platform.runLater {
          evs.foreach(appendEvent)
          updateHeader()
          renderHand()
          maybeShowOverlayOnTurnChange()
        }
      }

      updateAll()
    }
}
