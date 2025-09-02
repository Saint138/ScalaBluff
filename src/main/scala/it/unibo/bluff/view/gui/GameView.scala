package it.unibo.bluff.view.gui

import java.util.concurrent.atomic.AtomicReference

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.TurnOrder.given

import scalafx.Includes.*
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.geometry.Insets
import scalafx.animation.{Timeline, KeyFrame}
import scalafx.util.Duration

/** Game view now reads from a shared AtomicReference[GameState] so the timer can update the clocks and the UI
  * reflects the remaining time. It also shows a label and progress bar for the current player's remaining time.
  */
object GameView {

  def apply(stateRef: AtomicReference[GameState], maxPerTurnMs: Long = 60000L): BorderPane = new BorderPane {

    def currentState: GameState = stateRef.get()

    val lblTurn = new Label()
    val lblPile = new Label()
    val lblDecl = new Label()
    val lblClock = new Label()
    val clockBar = new ProgressBar { prefWidth = 200 }
    val log     = new TextArea { editable = false; prefRowCount = 8 }

    val cmbDeclared = new ComboBox[Rank](ObservableBuffer(Rank.values.toSeq*))
    val cmbRankHand = new ComboBox[Rank]()
    val cmbQty      = new ComboBox[Int]()

    val btnPlay = new Button("Gioca")
    val btnCall = new Button("Accusa Bluff")

    def updateQtyChoices(max: Int): Unit = {
      val items = if (max <= 0) Seq(1) else 1 to max
      cmbQty.items = ObservableBuffer(items*)
      if (items.nonEmpty) cmbQty.value = items.head
    }

    def updateUI(): Unit = {
      val st = currentState
      lblTurn.text = s"Turno: ${st.nameOf(st.turn)}"
      lblPile.text = s"Pila: ${st.pile.allCards.size} carte"
      lblDecl.text = "Ultima dichiarazione: " + st.lastDeclaration
        .map(d => s"${st.nameOf(d.player)} ${d.declared} (${d.hiddenCards.size})")
        .getOrElse("-")

      val remaining = st.clocks.getOrElse(st.turn, 0L)
      lblClock.text = f"Tempo rimasto: ${remaining / 1000.0}%.1f s"
      val frac = if (maxPerTurnMs > 0) remaining.toDouble / maxPerTurnMs.toDouble else 0.0
      clockBar.progress = math.max(0.0, math.min(1.0, frac))

      st.fixedDeclaredRank match {
        case Some(r) => cmbDeclared.value = r; cmbDeclared.disable = true
        case None =>
          if (cmbDeclared.value.value == null) cmbDeclared.value = Rank.Asso
          cmbDeclared.disable = false
      }

      val hand    = st.hands.getOrElse(st.turn, Hand.empty).cards
      val grouped = hand.groupBy(_.rank).view.mapValues(_.size).toMap
      cmbRankHand.items = ObservableBuffer(grouped.keys.toSeq.sortBy(_.ordinal)*)
      if (cmbRankHand.items().nonEmpty) {
        val cur = Option(cmbRankHand.value.value).getOrElse(cmbRankHand.items().head)
        cmbRankHand.value = cur
        updateQtyChoices(grouped(cur))
      }

      val gameOver = st.hands.exists(_._2.size == 0)
      btnPlay.disable = gameOver || hand.isEmpty
      btnCall.disable = gameOver || st.lastDeclaration.isEmpty
    }

    def step(cmd: GameCommand): Unit =
      val st = currentState
      Engine.step(st, cmd) match {
        case Left(err) =>
          new Alert(Alert.AlertType.Error) { headerText = "Mossa non valida"; contentText = err }.showAndWait()
        case Right((st2, evs)) =>
          stateRef.set(st2)
          evs.foreach {
            case Engine.GameEvent.Dealt(sz) =>
              log.appendText("Distribuite carte: " + sz.map{ case (p,s) => s"${st2.nameOf(p)}=$s" }.mkString(", ") + "\n")
            case Engine.GameEvent.Played(p, d, c) =>
              log.appendText(s"${st2.nameOf(p)} dichiara $d e gioca $c carte\n")
            case Engine.GameEvent.BluffCalled(by, ag, truth) =>
              log.appendText(s"${st2.nameOf(by)} accusa ${st2.nameOf(ag.player)} → " + (if truth then "VERA" else "FALSA") + "\n")
            case Engine.GameEvent.TimerExpired(p) =>
              log.appendText(s"Timeout: ${st2.nameOf(p)} ha perso tempo e prende la pila\n")
            case Engine.GameEvent.GameEnded(w) =>
              log.appendText(s"🏆 Vince ${st2.nameOf(w)}!\n")
            case _ => // altri eventi futuri
          }
          updateUI()
      }

    cmbRankHand.valueProperty.onChange { (_,_,r) =>
      if (r != null) {
        val count = currentState.hands.getOrElse(currentState.turn, Hand.empty).cards.count(_.rank == r)
        updateQtyChoices(count)
      }
    }

    btnPlay.onAction = _ => {
      val st = currentState
      val rHand = cmbRankHand.value.value
      if (rHand != null) {
        val qty   = Option(cmbQty.value.value).getOrElse(1)
        val cards = st.hands.getOrElse(st.turn, Hand.empty).cards.filter(_.rank == rHand).take(qty)
        val decl  = Option(st.fixedDeclaredRank).flatten.getOrElse(cmbDeclared.value.value)
        step(GameCommand.Play(st.turn, cards, decl))
      }
    }

    btnCall.onAction = _ => step(GameCommand.CallBluff(currentState.turn))

    val playGrid = new GridPane {
      hgap = 10; vgap = 8; padding = Insets(10)
      add(new Label("Rango dichiarato:"), 0, 0); add(cmbDeclared, 1, 0)
      add(new Label("Rango in mano:"),    0, 1); add(cmbRankHand, 1, 1)
      add(new Label("Quantità:"),         0, 2); add(cmbQty,      1, 2)
      add(btnPlay, 1, 3)
    }
    val playPane = new TitledPane { text = "Giocata"; content = playGrid; collapsible = false }
    val logPane  = new TitledPane { text = "Log";     content = log;      collapsible = false }

    top    = new VBox(6, lblTurn, lblPile, lblDecl, new HBox(8, lblClock, clockBar)) { padding = Insets(10) }
    center = new VBox(10, playPane, new HBox(10, btnCall), logPane) { padding = Insets(10) }

    // Timeline che aggiorna la UI ogni 200ms per riflettere il countdown
    val timeline = Timeline(KeyFrame(Duration(200), onFinished = _ => updateUI()))
    timeline.cycleCount = Timeline.Indefinite
    timeline.play()

    updateUI()
  }
}
