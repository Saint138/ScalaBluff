package gui

import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.text.Text

object GameView:
  def apply(): BorderPane =
    new BorderPane {
      top = new Text("ScalaBluff - Game Table")
      center = new Text("Pila centrale: (vuota)")
      //bottom = HandView()
      //right = ControlsView()
    }
