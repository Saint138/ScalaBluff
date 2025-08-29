package it.unibo.bluff.view.gui

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
//game view when pressing start game button. to be fixed yet
object GameView {

  def apply(initial: GameState): BorderPane = new BorderPane {
    private var st: GameState = initial

    val lblTurn = new Label()
    val lblPile = new Label()
    val lblDecl = new Label()
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
      lblTurn.text = s"Turno: ${st.nameOf(st.turn)}"
      lblPile.text = s"Pila: ${st.pile.allCards.size} carte"
      lblDecl.text = "Ultima dichiarazione: " + st.lastDeclaration
        .map(d => s"${st.nameOf(d.player)} ${d.declared} (${d.hiddenCards.size})")
        .getOrElse("-")

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
      Engine.step(st, cmd) match {
        case Left(err) =>
          new Alert(Alert.AlertType.Error) { headerText = "Mossa non valida"; contentText = err }.showAndWait()
        case Right((st2, evs)) =>
          st = st2
          evs.foreach {
            case Engine.GameEvent.Dealt(sz) =>
              log.appendText("Distribuite carte: " + sz.map{ case (p,s) => s"${st.nameOf(p)}=$s" }.mkString(", ") + "\n")
            case Engine.GameEvent.Played(p, d, c) =>
              log.appendText(s"${st.nameOf(p)} dichiara $d e gioca $c carte\n")
            case Engine.GameEvent.BluffCalled(by, ag, truth) =>
              log.appendText(s"${st.nameOf(by)} accusa ${st.nameOf(ag.player)} → " + (if truth then "VERA" else "FALSA") + "\n")
            case Engine.GameEvent.GameEnded(w) =>
              log.appendText(s"🏆 Vince ${st.nameOf(w)}!\n")
          }
          updateUI()
      }

    cmbRankHand.valueProperty.onChange { (_,_,r) =>
      if (r != null) {
        val count = st.hands.getOrElse(st.turn, Hand.empty).cards.count(_.rank == r)
        updateQtyChoices(count)
      }
    }

    btnPlay.onAction = _ => {
      val rHand = cmbRankHand.value.value
      if (rHand != null) {
        val qty   = Option(cmbQty.value.value).getOrElse(1)
        val cards = st.hands.getOrElse(st.turn, Hand.empty).cards.filter(_.rank == rHand).take(qty)
        val decl  = Option(st.fixedDeclaredRank).flatten.getOrElse(cmbDeclared.value.value)
        step(GameCommand.Play(st.turn, cards, decl))
      }
    }

    btnCall.onAction = _ => step(GameCommand.CallBluff(st.turn))

    val playGrid = new GridPane {
      hgap = 10; vgap = 8; padding = Insets(10)
      add(new Label("Rango dichiarato:"), 0, 0); add(cmbDeclared, 1, 0)
      add(new Label("Rango in mano:"),    0, 1); add(cmbRankHand, 1, 1)
      add(new Label("Quantità:"),         0, 2); add(cmbQty,      1, 2)
      add(btnPlay, 1, 3)
    }
    val playPane = new TitledPane { text = "Giocata"; content = playGrid; collapsible = false }
    val logPane  = new TitledPane { text = "Log";     content = log;      collapsible = false }

    top    = new VBox(6, lblTurn, lblPile, lblDecl) { padding = Insets(10) }
    center = new VBox(10, playPane, new HBox(10, btnCall), logPane) { padding = Insets(10) }

    updateUI()
  }
}
