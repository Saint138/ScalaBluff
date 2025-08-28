package it.unibo.bluff.view.gui

import scalafx.Includes._
import scalafx.scene.control.Button
import scalafx.scene.layout.VBox

object MainMenuView {

  def apply(onNew: () => Unit,
            onRules: () => Unit,
            onStats: () => Unit): VBox = new VBox {
    spacing = 10
    children = Seq(
      new Button("Nuovo gioco")   { onAction = _ => onNew() },
      new Button("Regole")        { onAction = _ => onRules() },
      new Button("Statistiche")   { onAction = _ => onStats() }
    )
  }
}
