package it.unibo.bluff.view.gui.components

import it.unibo.bluff.model.state.GameState
import scala.reflect.Selectable.reflectiveSelectable
import scalafx.scene.layout.{BorderPane, HBox, VBox, Region}
import scalafx.scene.control.{Label, ProgressBar}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

object HeaderBar {
  def apply(): HeaderBar = new HeaderBar()

  final class HeaderBar extends BorderPane {
    private val lblTurn  = new Label() { font = Font.font("System", FontWeight.Bold, 14) }
    private val lblPile  = new Label()
    private val lblDecl  = new Label()
    private val lblClock = new Label() { font = Font.font("System", FontWeight.Bold, 14) }
    private val clockBar = new ProgressBar { prefWidth = 160 }
    private val lblMatch = new Label()

    private val appMark = new Region {
      prefWidth = 16; prefHeight = 16
      style = "-fx-background-color:#ef8354; -fx-background-radius:4;"
    }
    private val titleLbl = new Label("ScalaBluff") {
      font = Font.font("Verdana", FontWeight.Bold, 28)
      textFill = Color.web("#2d3142")
    }

    left = new VBox(4,
      new HBox(10, appMark, titleLbl) { alignment = Pos.CenterLeft },
      lblTurn, lblPile, lblDecl
    ) { alignment = Pos.CenterLeft }

    right = new VBox(6,
      new HBox(10, lblClock, clockBar) { alignment = Pos.CenterRight },
      new HBox(new Label("Partita:"), new Region { prefWidth = 8 }, lblMatch) { alignment = Pos.CenterRight }
    ) { alignment = Pos.CenterRight }

    padding = Insets(10, 16, 4, 16)

    def update(st: GameState, matchStart: Long, maxPerTurnMs: Long): Unit = {
      lblTurn.text = s"Turno: ${st.nameOf(st.turn)}"
      lblPile.text = s"Pila: ${st.pile.allCards.size} carte"
      lblDecl.text = st.lastDeclaration
        .map(d => s"Ultima dichiarazione: ${st.nameOf(d.player)} → ${d.declared} (${d.hiddenCards.size})")
        .getOrElse("Ultima dichiarazione: -")

      val rem: Long =
        try st.asInstanceOf[ {def clocks: Map[it.unibo.bluff.model.PlayerId, Long]}].clocks.getOrElse(st.turn, 0L)
        catch {
          case _: Throwable => 0L
        }

      lblClock.text = f"${rem / 1000.0}%.1f s"
      clockBar.progress = if maxPerTurnMs > 0 then math.max(0.0, math.min(1.0, rem.toDouble / maxPerTurnMs)) else 0.0

      val elapsed = System.currentTimeMillis() - matchStart
      val totalSec = elapsed / 1000
      lblMatch.text = f"${totalSec / 60}%02d:${totalSec % 60}%02d"
    }
  }
}
