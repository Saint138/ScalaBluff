package it.unibo.bluff.view.gui

import java.util.concurrent.atomic.AtomicReference

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.view.gui.components.*
import it.unibo.bluff.model.bot.BotManager

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

  /** La View non chiama più Engine.step: riceve una funzione di dispatch dal Controller. */
  def apply(
    stateRef: AtomicReference[GameState],
    maxPerTurnMs: Long = 60_000L,
    dispatch: GameCommand => Either[String, List[GameEvent]],
    onGameEnded: PlayerId => Unit = _ => (),
    onExitToMenu: () => Unit = () => (),
    onOverlayChange: Boolean => Unit = _ => () // true=overlay ON, false=OFF
  ): BorderPane =
    new BorderPane {

      private def st: GameState = stateRef.get()

      // ===== Stato selezione carte =====
      private val selected  = mutable.LinkedHashSet.empty[Card]
      private val handNodes = mutable.Buffer.empty[CardNode.CardNode]

      private def clearSelectionVisual(): Unit =
        handNodes.foreach(_.markSelected(false))

      private def resetSelection(): Unit =
        selected.clear()
        clearSelectionVisual()
        updateButtonsEnabled()

      // ===== Componenti =====
      private val header    = HeaderBar()
      private val handPane  = HandPanel()
      private val logArea   = LogPanel()
      private val actions   = ActionsPanel()

      // (opzionale) leggero boost visivo ai bottoni azione
      actions.style = "-fx-font-size: 14px;"

      // ===== Overlay privacy tra turni (solo 2 umani) =====
      private var currentViewer: PlayerId = st.turn
      private var overlayShown: Boolean = false
      private var gameEnded: Boolean = false

      // Contenuto centrale reale (layout invariato)
      private val centerContent = new HBox(16,
        new VBox(12, handPane) { padding = Insets(10) },
        new VBox(8, actions, logArea) { padding = Insets(10) }
      ) { padding = Insets(10, 16, 10, 16) }

      // Overlay UI (evidente e centrato)
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

      // StackPane per sovrapporre overlay al contenuto
      center = new StackPane {
        children = Seq(centerContent, overlayPane)
      }

      // ===== Toggle selezione carta =====
      private def toggleSelect(n: CardNode.CardNode): Unit =
        val c = n.card
        if selected.contains(c) then
          selected.remove(c)
          n.markSelected(false)
        else
          selected.add(c)      // ← rimosso il limite a 3, ora puoi selezionare 4 (o più) carte
          n.markSelected(true)
        updateButtonsEnabled()

      // ===== Render mano =====
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

      // ===== Update UI =====
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

      // ===== Logger eventi in GameView (anche per bot) =====
      private def appendEvent(ev: Engine.GameEvent): Unit = ev match {
        case Engine.GameEvent.Dealt(sz) =>
          logArea.appendText("Distribuite carte: " + sz.map { case (p, s) => s"${st.nameOf(p)}=$s" }.mkString(", ") + "\n")
        case Engine.GameEvent.Played(p, d, c) =>
          logArea.appendText(s"${st.nameOf(p)} dichiara $d e gioca $c carte\n")
        case Engine.GameEvent.BotPlayed(p, d, c) =>
          logArea.appendText(s"(BOT) ${st.nameOf(p)} dichiara $d e gioca $c carte\n")
        case Engine.GameEvent.BluffCalled(by, ag, truth) =>
          logArea.appendText(s"${st.nameOf(by)} accusa ${st.nameOf(ag.player)} → " + (if truth then "VERA" else "FALSA") + "\n")
        case Engine.GameEvent.TimerExpired(p) =>
          logArea.appendText(s"Timeout: ${st.nameOf(p)} ha esaurito il tempo.\n")
        case Engine.GameEvent.QuartetCleared(p, r, cnt) =>
          logArea.appendText(s"♻️ ${st.nameOf(p)} elimina automaticamente $cnt carte ($r)\n")
        case Engine.GameEvent.GameEnded(w) =>
          logArea.appendText(s"🏆 Vince ${st.nameOf(w)}!\n")
          uiTick.stop()
      }

      // ===== Actions (via Controller.dispatch) =====
      private def send(cmd: GameCommand): Unit =
        dispatch(cmd) match
          case Left(err) =>
            new Alert(Alert.AlertType.Error) { headerText = "Mossa non valida"; contentText = err }.showAndWait()
          case Right(evs) =>
            val st2 = stateRef.get()
            evs.foreach {
              case GameEvent.Dealt(sz) =>
                logArea.appendText("Distribuite carte: " + sz.map { case (p, s) => s"${st2.nameOf(p)}=$s" }.mkString(", ") + "\n")
              case GameEvent.Played(p, d, c) =>
                logArea.appendText(s"${st2.nameOf(p)} dichiara $d e gioca $c carte\n")
              case GameEvent.BluffCalled(by, ag, truth) =>
                logArea.appendText(s"${st2.nameOf(by)} accusa ${st2.nameOf(ag.player)} → " + (if truth then "VERA" else "FALSA") + "\n")
              case GameEvent.TimerExpired(p) =>
                logArea.appendText(s"Timeout: ${st2.nameOf(p)} ha esaurito il tempo.\n")
              case GameEvent.QuartetCleared(p, r, cnt) =>
                logArea.appendText(s"♻️ ${st2.nameOf(p)} elimina automaticamente $cnt carte ($r)\n")
              case GameEvent.GameEnded(w) =>
                logArea.appendText(s"🏆 Vince ${st2.nameOf(w)}!\n")
                uiTick.stop()
                gameEnded = true
                if overlayShown then hideOverlay()
                onGameEnded(w)
              case _ => ()
            }
            updateAll()

      actions.onPlay { decl =>
        val toPlay = selected.toList
        if toPlay.nonEmpty && decl != null then
          send(GameCommand.Play(st.turn, toPlay, decl))
          resetSelection()
      }
      actions.onCall { send(GameCommand.CallBluff(st.turn)) }
      actions.onClear { resetSelection() }

      // ===== Tick UI =====
      private val uiTick = Timeline(KeyFrame(Duration(200), onFinished = _ => updateHeader()))
      uiTick.cycleCount = Timeline.Indefinite
      uiTick.play()

      // ===== Sottoscrizione agli eventi del bot (e di chiunque chiami onEvents) =====
      BotManager.onEvents = { evs =>
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
