package it.unibo.bluff.view.gui.components

import scalafx.scene.layout.{FlowPane, StackPane, VBox}
import scalafx.scene.control.Label
import scalafx.geometry.Insets

object HandPanel {
  def apply(): FlowPane = new FlowPane { hgap = 10; vgap = 10 }
}
