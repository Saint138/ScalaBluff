package it.unibo.bluff.view.gui

import java.util.concurrent.atomic.AtomicReference

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.view.gui.components.*
import it.unibo.bluff.model.bot.BotManager

import scalafx.Includes.*
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.util.Duration

import scala.collection.mutable
import scala.reflect.Selectable.reflectiveSelectable

object GameView {

  def apply(stateRef: AtomicReference[GameState], maxPerTurnMs: Long = 60_000L): BorderPane =
    new BorderPane {

      private def st: GameState = stateRef.get()

      // ===== Stato selezione carte =====
      private val selected  = mutable.LinkedHashSet.empty[Card]
      private val handNodes = mutable.Buffer.empty[CardNode.CardNode]

      private def clearSelectionVisual(): Unit =
        handNodes.foreach(_.markSelected(false))

      private def resetSelection(): Unit = {
        selected.clear()
        clearSelectionVisual()
        updateButtonsEnabled()
      }

      // ===== Componenti =====
      private val header    = HeaderBar()
      private val handPane  = HandPanel()
      private val logArea   = LogPanel()
      private val actions   = ActionsPanel()

      top = header
      center = new HBox(16,
        new VBox(12, handPane, logArea) { padding = Insets(10) },
        actions
      ) { padding = Insets(10, 16, 10, 16) }

      // ===== Toggle selezione carta =====
      private def toggleSelect(n: CardNode.CardNode): Unit = {
        val c = n.card
        if (selected.contains(c)) {
          selected.remove(c)
          n.markSelected(false)
        } else if (selected.size < 3) {       // (se vuoi max 4 cambia 3 -> 4)
          selected.add(c)
          n.markSelected(true)
        }
        updateButtonsEnabled()
      }

      // ===== Render mano =====
      private def renderHand(): Unit = {
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
      }

      // ===== Update UI =====
      private val matchStart = System.currentTimeMillis()
      private def updateHeader(): Unit =
        header.update(st, matchStart, maxPerTurnMs)

      private def updateButtonsEnabled(): Unit =
        actions.updateButtons(st, selected.nonEmpty)

      private def updateAll(): Unit = { updateHeader(); renderHand() }

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
        case Engine.GameEvent.GameEnded(w) =>
          logArea.appendText(s"🏆 Vince ${st.nameOf(w)}!\n")
      }

      // ===== Actions =====
      actions.onPlay { decl =>
        val toPlay = selected.toList
        if (toPlay.nonEmpty && decl != null) {
          step(GameCommand.Play(st.turn, toPlay, decl))
          resetSelection()
        }
      }
      actions.onCall { step(GameCommand.CallBluff(st.turn)) }
      actions.onClear { resetSelection() }

      private def step(cmd: GameCommand): Unit = {
        Engine.step(st, cmd) match
          case Left(err) =>
            new Alert(Alert.AlertType.Error) { headerText = "Mossa non valida"; contentText = err }.showAndWait()
          case Right((st2, evs)) =>
            stateRef.set(st2)
            // inoltra gli eventi sul canale condiviso (GameView li riceve via onEvents)
            BotManager.onEvents(evs)
            updateAll()
      }

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
        }
      }

      updateAll()
    }
}
