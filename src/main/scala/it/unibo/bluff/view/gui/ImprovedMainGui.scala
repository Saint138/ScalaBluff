package it.unibo.bluff.view.gui

import it.unibo.bluff.coordinator.GameCoordinator
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.stage.StageStyle

/**
 * Entry point dell'applicazione GUI
 * Delega tutto al GameCoordinator
 */
object ImprovedMainGUI extends JFXApp3 {

  override def start(): Unit = {
    // Crea il coordinatore
    val coordinator = GameCoordinator()
    
    // Crea la finestra principale
    stage = new JFXApp3.PrimaryStage {
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 1100
      height = 720
      
      scene = new Scene(width.value, height.value) {
        root = MainMenuView(
          onNewGame = () => coordinator.showNewGameDialog(stage),
          onRules = () => coordinator.showRules(stage)
        )
      }
    }
    
    // Centra la finestra
    stage.centerOnScreen()
    
    // Gestisce la chiusura
    stage.onCloseRequest = _ => {
      coordinator.shutdown()
    }
  }
}