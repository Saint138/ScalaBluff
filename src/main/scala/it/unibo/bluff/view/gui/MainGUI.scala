package gui

import it.unibo.bluff.view.gui.{MainMenuView, NewGameDialog, GameView}
import it.unibo.bluff.setup.GameSetup
import it.unibo.bluff.model.state.GameState
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
      scene = new Scene:
        content = MainMenuView(
          onNew = () => {
            NewGameDialog.askPlayers().foreach { names =>
              val (stDealt, _events, _deckSize) = GameSetup.fairInitialDeal(names.size, names)
              val root = new BorderPane()
              root.setCenter(GameView(stDealt))
              this.setRoot(root)
            }
          },
          onRules = () => println("Mostra regole..."),
          onStats = () => println("Mostra statistiche...")
        )
