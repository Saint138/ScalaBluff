package it.unibo.bluff.view.gui

import java.util.concurrent.atomic.AtomicReference

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.TurnOrder.given

import scalafx.Includes.*
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}
import scalafx.util.Duration
import scalafx.scene.image.{Image, ImageView}


import scala.collection.mutable
import scala.reflect.Selectable.reflectiveSelectable

object GameView {

  /** Vista principale del tavolo di gioco (stile mockup).
    * @param stateRef stato condiviso (aggiornato da Engine.step)
    * @param maxPerTurnMs tempo massimo per turno (progress bar)
    */
  def apply(stateRef: AtomicReference[GameState], maxPerTurnMs: Long = 60_000L): BorderPane =
    new BorderPane {

      // ===== Helpers stato/UI =====
      private def st: GameState = stateRef.get()

      // selezione carte (max 4, stesso rango)
      private val selected  = mutable.LinkedHashSet.empty[Card]
      private val handNodes = mutable.Buffer.empty[CardNode]

      private def clearSelectionVisual(): Unit =
        handNodes.foreach(_.markSelected(false))

      private def resetSelection(): Unit = {
        selected.clear()
        clearSelectionVisual()
        updateButtonsEnabled()
      }

      // ===== Top bar =====
      private val appMark = new Region {
        prefWidth = 16; prefHeight = 16
        style = "-fx-background-color:#ef8354; -fx-background-radius:4;"
      }
      private val titleLbl = new Label("ScalaBluff") {
        font = Font.font("Verdana", FontWeight.Bold, 28)
        textFill = Color.web("#2d3142")
        style = "-fx-effect: dropshadow(gaussian, #bfc0c0, 8, 0, 2, 2);"
      }
      private val lblTurn  = new Label() { font = Font.font("System", FontWeight.Bold, 14) }
      private val lblPile  = new Label()
      private val lblDecl  = new Label()
      private val lblClock = new Label() { font = Font.font("System", FontWeight.Bold, 14) }
      private val clockBar = new ProgressBar { prefWidth = 160 }
      private val lblMatch = new Label()

      private val leftTop  = new VBox(4,
        new HBox(10, appMark, titleLbl) { alignment = Pos.CenterLeft },
        lblTurn, lblPile, lblDecl
      ) { alignment = Pos.CenterLeft }

      private val rightTop = new VBox(6,
        new HBox(10, lblClock, clockBar) { alignment = Pos.CenterRight },
        new HBox(new Label("Partita:"), new Region { prefWidth = 8 }, lblMatch) { alignment = Pos.CenterRight }
      ) { alignment = Pos.CenterRight }

      top = new BorderPane {
        padding = Insets(10, 16, 4, 16)
        left  = leftTop
        right = rightTop
      }

      // ===== Sezioni/pannelli riutilizzabili =====
      private def section(title: String, content: Region): VBox =
        new VBox(
          new Label(title) {
            font = Font.font("System", FontWeight.Bold, 14)
            textFill = Color.web("#2d3142")
          },
          new StackPane {
            padding = Insets(10)
            children = content
            style =
              "-fx-background-color: white; -fx-background-radius:10; -fx-border-radius:10; -fx-border-color:#d7d7d7;"
          }
        ) { spacing = 6 }

      // ===== Pannello sinistro (mano su tavolo + log) =====
      // Tavolo verde
      private val tableFelt = new StackPane {
        padding = Insets(12)
        style = "-fx-background-color: #2e7d67; -fx-background-radius:10;"
      }
      private val handPane = new FlowPane { hgap = 10; vgap = 10 }

      tableFelt.children = handPane
      private val handSection = section("Mano", tableFelt)

      private val logArea = new TextArea {
        editable = false
        prefRowCount = 8
        style = "-fx-font-size: 13;"
      }
      private val logSection = section("Log", logArea)

      private val leftColumn = new VBox(12, handSection, logSection) {
        padding = Insets(10)
      }

      // ===== Pannello destro (azioni) =====
      private val cmbDeclared = new ComboBox[Rank](ObservableBuffer(Rank.values.toSeq*))

      private val btnPlay = new Button("Gioca") {
        prefWidth = 200; prefHeight = 40
        style = "-fx-background-color:#ef8354; -fx-text-fill:white; -fx-font-weight:bold; -fx-font-size:14;"
      }
      private val btnCall = new Button("Accusa Bluff") {
        prefWidth = 200; prefHeight = 40
        style = "-fx-background-color:#4f5d75; -fx-text-fill:white; -fx-font-weight:bold; -fx-font-size:14;"
      }
      private val btnClear = new Button("Annulla selezione") {
        prefWidth = 200
      }

      private val actionsPane = new VBox(12,
        new VBox(6, new Label("Rango dichiarato:"), cmbDeclared),
        btnPlay,
        btnCall,
        btnClear
      ) {
        alignment = Pos.TopCenter
        padding = Insets(12)
        style = "-fx-background-color:#f7f7f8; -fx-background-radius:10; -fx-border-radius:10; -fx-border-color:#e5e5e7;"
      }

      center = new HBox(16,
        new VBox(leftColumn) { VBox.setVgrow(this, Priority.ALWAYS); HBox.setHgrow(this, Priority.ALWAYS) },
        new VBox(actionsPane) { prefWidth = 260 }
      ) { padding = Insets(10, 16, 10, 16) }

      // ===== Carta cliccabile =====
      private val baseStyle =
        "-fx-background-color: white; -fx-background-radius:12; -fx-border-radius:12;" +
          "-fx-border-color:#bfc0c0; -fx-effect:dropshadow(gaussian, rgba(0,0,0,0.10), 6, 0, 1, 1);"
      private val selectedStyle =
        "-fx-background-color: white; -fx-background-radius:12; -fx-border-radius:12;" +
          "-fx-border-color:#ef8354; -fx-border-width:3; -fx-effect:dropshadow(gaussian, rgba(239,131,84,0.35), 10, 0, 0, 0);"

      private def suitSymbol(s: Suit): String =
        s.toString.toLowerCase match
          case "cuori" | "hearts"   => "♥"
          case "quadri" | "diamonds"=> "♦"
          case "fiori"  | "clubs"   => "♣"
          case "picche" | "spades"  => "♠"
          case _                    => s.toString

      private def suitColor(s: Suit): Color =
        s.toString.toLowerCase match
          case "cuori" | "hearts" | "quadri" | "diamonds" => Color.web("#ef8354")
          case _                                          => Color.web("#2d3142")

      final class CardNode(val card: Card) extends StackPane {
        minWidth = 82; prefWidth = 82; maxWidth = 82
        minHeight = 116; prefHeight = 116; maxHeight = 116
        padding = Insets(4)
        style = baseStyle

        // Prova sempre a caricare l'immagine della carta
        private val maybeUrl = Option(getClass.getResource(imagePath(card)))

        children = maybeUrl match
          case Some(url) =>
            println(s"Carico immagine: ${imagePath(card)} → $url")
            new ImageView(new Image(url.toExternalForm)) {
              fitWidth = 72
              fitHeight = 108
              preserveRatio = true
            }

          case None =>
            new Label(s"${card.rank} ${suitSymbol(card.suit)}") {
              font = Font.font("System", FontWeight.Bold, 14)
              textFill = suitColor(card.suit)
            }

        def markSelected(on: Boolean): Unit =
          style = if on then selectedStyle else baseStyle

        onMouseClicked = _ => toggleSelect(this)
      }

      private def toggleSelect(n: CardNode): Unit = {
        val c = n.card
        if selected.contains(c) then {
          selected.remove(c); n.markSelected(false)
        } else {
          if selected.isEmpty then {
            selected.add(c); n.markSelected(true)
            if st.fixedDeclaredRank.isEmpty && cmbDeclared.value.value == null then cmbDeclared.value = c.rank
          } else {
            val r0 = selected.head.rank
            if c.rank == r0 && selected.size < 4 then {
              selected.add(c); n.markSelected(true)
            } else {
              // rango diverso → reset e nuova selezione
              resetSelection()
              selected.add(c); n.markSelected(true)
              if st.fixedDeclaredRank.isEmpty then cmbDeclared.value = c.rank
            }
          }
        }
        updateButtonsEnabled()
      }

      private def renderHand(): Unit = {
        handPane.children.clear()
        handNodes.clear()
        val cards = st.hands.getOrElse(st.turn, Hand.empty).cards
          .sortBy(c => (c.rank.ordinal, c.suit.ordinal))
        cards.foreach { c =>
          val node = new CardNode(c)
          handNodes += node
          handPane.children.add(node)
        }
        updateButtonsEnabled()
      }

      // ===== Update UI =====
      private def fmtMs(ms: Long): String = f"${ms / 1000.0}%.1f s"
      private val matchStart = System.currentTimeMillis()

      private def updateHeader(): Unit = {
        lblTurn.text = s"Turno: ${st.nameOf(st.turn)}"
        lblPile.text = s"Pila: ${st.pile.allCards.size} carte"
        lblDecl.text = st.lastDeclaration
          .map(d => s"Ultima dichiarazione: ${st.nameOf(d.player)} → ${d.declared} (${d.hiddenCards.size})")
          .getOrElse("Ultima dichiarazione: -")

        // timer turno (se usi st.clocks: Map[PlayerId, Long])
        val rem: Long =
          try st.asInstanceOf[{ def clocks: Map[PlayerId, Long] }].clocks.getOrElse(st.turn, 0L)
          catch { case _: Throwable => 0L }
        lblClock.text = s"Tempo rimasto: ${fmtMs(rem)}"
        clockBar.progress = if maxPerTurnMs > 0 then math.max(0.0, math.min(1.0, rem.toDouble / maxPerTurnMs)) else 0.0

        val elapsed = System.currentTimeMillis() - matchStart
        val totalSec = elapsed / 1000
        lblMatch.text = f"${totalSec / 60}%02d:${totalSec % 60}%02d"

        st.fixedDeclaredRank match
          case Some(r) => cmbDeclared.value = r; cmbDeclared.disable = true
          case None =>
            if Option(cmbDeclared.value.value).isEmpty then cmbDeclared.value = Rank.Asso
            cmbDeclared.disable = false
      }

      private def updateButtonsEnabled(): Unit = {
        val handEmpty = st.hands.getOrElse(st.turn, Hand.empty).cards.isEmpty
        val gameOver  = st.hands.exists(_._2.size == 0)
        btnPlay.disable = gameOver || handEmpty || selected.isEmpty
        btnCall.disable = gameOver || st.lastDeclaration.isEmpty
      }

      private def updateAll(): Unit = { updateHeader(); renderHand() }

      // ===== Actions =====
      btnPlay.onAction = _ => {
        val decl = Option(st.fixedDeclaredRank).flatten.getOrElse(cmbDeclared.value.value)
        val toPlay = selected.toList
        if (toPlay.nonEmpty && decl != null) {
          step(GameCommand.Play(st.turn, toPlay, decl))
          resetSelection()
        }
      }
      btnCall.onAction = _ => step(GameCommand.CallBluff(st.turn))
      btnClear.onAction = _ => resetSelection()

      private def step(cmd: GameCommand): Unit = {
        Engine.step(st, cmd) match
          case Left(err) =>
            new Alert(Alert.AlertType.Error) { headerText = "Mossa non valida"; contentText = err }.showAndWait()
          case Right((st2, evs)) =>
            stateRef.set(st2)
            evs.foreach {
              case Engine.GameEvent.Dealt(sz) =>
                logArea.appendText("Distribuite carte: " + sz.map { case (p, s) => s"${st2.nameOf(p)}=$s" }.mkString(", ") + "\n")
              case Engine.GameEvent.Played(p, d, c) =>
                logArea.appendText(s"${st2.nameOf(p)} dichiara $d e gioca $c carte\n")
              case Engine.GameEvent.BluffCalled(by, ag, truth) =>
                logArea.appendText(s"${st2.nameOf(by)} accusa ${st2.nameOf(ag.player)} → " + (if truth then "VERA" else "FALSA") + "\n")
              case Engine.GameEvent.TimerExpired(p) =>
                logArea.appendText(s"Timeout: ${st2.nameOf(p)} ha esaurito il tempo.\n")
              case Engine.GameEvent.GameEnded(w) =>
                logArea.appendText(s"🏆 Vince ${st2.nameOf(w)}!\n")
              case _ => ()
            }
            updateAll()
      }

      // ===== Tick UI (no wall clock nell’engine) =====
      private val uiTick = Timeline(KeyFrame(Duration(200), onFinished = _ => updateHeader()))
      uiTick.cycleCount = Timeline.Indefinite
      uiTick.play()

      // init
      updateAll()
    }


  private def imagePath(card: Card): String =
    val rankStr = card.rank.toString.toLowerCase // esempio: "asso", "due", "re"
    val suitStr = card.suit.toString.toLowerCase // esempio: "cuori", "picche"
    s"/cards/${rankStr}_${suitStr}.jpeg"

}
