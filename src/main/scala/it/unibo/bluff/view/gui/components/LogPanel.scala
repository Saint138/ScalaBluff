package it.unibo.bluff.view.gui.components

import scalafx.scene.control.TextArea
import scalafx.scene.layout.VBox
import scalafx.scene.control.Label

object LogPanel {
  def apply(): TextArea =
    new TextArea {
      editable = false
      prefRowCount = 8
      style = "-fx-font-size: 13;"
    }
}
