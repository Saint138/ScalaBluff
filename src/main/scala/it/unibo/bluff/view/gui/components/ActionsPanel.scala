package it.unibo.bluff.view.gui.components

import it.unibo.bluff.model.{Rank, Hand}
import it.unibo.bluff.model.state.GameState
import scalafx.scene.control.{Button, ComboBox, Label}
import scalafx.scene.layout.VBox
import scalafx.collections.ObservableBuffer

object ActionsPanel {
  def apply(): ActionsPanel = new ActionsPanel()

  final class ActionsPanel extends VBox(12) {
    val cmbDeclared = new ComboBox[Rank](ObservableBuffer(Rank.values.toSeq*))
    val btnPlay  = new Button("Gioca")
    val btnCall  = new Button("Accusa Bluff")
    val btnClear = new Button("Annulla selezione")

    children = Seq(
      new VBox(6, new Label("Rango dichiarato:"), cmbDeclared),
      btnPlay, btnCall, btnClear
    )

    // callback wiring
    private var playHandler: Rank => Unit = _ => ()
    private var callHandler: () => Unit   = () => ()
    private var clearHandler: () => Unit  = () => ()

    def onPlay(f: Rank => Unit): Unit = btnPlay.onAction = _ => f(cmbDeclared.value.value)
    def onCall(f: => Unit): Unit = btnCall.onAction = _ => f
    def onClear(f: => Unit): Unit = btnClear.onAction = _ => f

    def updateButtons(st: GameState, hasSelection: Boolean): Unit = {
      val handEmpty = st.hands.getOrElse(st.turn, Hand.empty).cards.isEmpty
      val gameOver  = st.hands.exists(_._2.size == 0)
      btnPlay.disable = gameOver || handEmpty || !hasSelection
      btnCall.disable = gameOver || st.lastDeclaration.isEmpty
    }
  }
}
