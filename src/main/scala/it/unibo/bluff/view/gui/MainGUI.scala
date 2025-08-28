package gui

import it.unibo.bluff.view.gui.MainMenuView
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.stage.StageStyle

object MainGUI extends JFXApp3:
  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 400
      height = 300
      scene = new Scene:
        content = MainMenuView (
          onNew = () => println("Avvia nuovo gioco..."),
          onRules = () => println("Mostra regole..."),
          onStats = () => println("Mostra statistiche...")
        )
