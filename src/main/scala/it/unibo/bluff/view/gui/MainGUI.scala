package gui

import it.unibo.bluff.view.gui.{MainMenuView, NewGameDialog, GameView}
import it.unibo.bluff.setup.GameSetup
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.stage.StageStyle

object MainGUI extends JFXApp3:
  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 800
      height = 600
      scene = new Scene(width.value, height.value):
        root = MainMenuView(
          onNewGame = () =>
            NewGameDialog.askPlayers().foreach { names =>
              val (stDealt, _, _) = GameSetup.fairInitialDeal(names.size, names)
              root = new BorderPane:
                center = it.unibo.bluff.view.gui.GameView(stDealt)
            },
          onRules = () => println("Mostra regole..."),
          onStats = () => println("Mostra statistiche...")
        )
  stage.centerOnScreen()
