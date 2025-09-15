package it.unibo.bluff.view.gui

import scalafx.application.Platform
import scalafx.scene.control.{Alert, TextArea}

object StatsDialog {
  def show(header: String, content: String): Unit = {
    Platform.runLater {
      val dlg = new Alert(Alert.AlertType.Information) {
        headerText = header
        dialogPane().setContent(new TextArea {
          editable = false
          wrapText = true
          text = content
          prefColumnCount = 60
          prefRowCount = 18
        })
      }
      dlg.showAndWait()
    }
  }
}
