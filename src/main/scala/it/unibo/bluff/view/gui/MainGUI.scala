package it.unibo.bluff.view.gui

import it.unibo.bluff.controller.GUIController
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.stage.StageStyle
import it.unibo.bluff.view.gui.{MainMenuView, NewGameDialog, RulesDialog}

object MainGUI extends JFXApp3 {

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage {
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 1100
      height = 720
      scene = new Scene(width.value, height.value) {
        root = MainMenuView(
          onNewGame = () => GUIController.onNewGame(stage),
          onRules   = () => GUIController.onRules(stage)
        )
      }
    }
    stage.centerOnScreen()
    stage.onCloseRequest = _ => {
      GUIController.shutdown()
    }
}
