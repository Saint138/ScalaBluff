package it.unibo.bluff.view.gui

import scalafx.Includes._
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{VBox, StackPane}
import scalafx.geometry.Pos
import scalafx.scene.text.{Font, FontWeight}
import scalafx.scene.paint.Color



object MainMenuView {

  def apply(
    onNewGame: () => Unit,
    onRules: () => Unit,
    onStats: () => Unit
  ): StackPane = {
    val title = new Label("ScalaBluff") {
      font = Font.font("Verdana", FontWeight.Bold, 40)
      textFill = Color.web("#2d3142")
      alignment = Pos.Center
      style = "-fx-effect: dropshadow(gaussian, #bfc0c0, 8, 0, 2, 2);"
    }

    val buttonBox = new VBox {
      spacing = 15
      alignment = Pos.Center
      children = Seq(
        new Button("Nuovo gioco") {
          minWidth = 200
          font = Font.font("Arial", FontWeight.Bold, 18)
          style = "-fx-background-color: #ef8354; -fx-text-fill: white;"
          onAction = _ => onNewGame()
        },
        new Button("Regole") {
          minWidth = 200
          font = Font.font("Arial", FontWeight.Bold, 18)
          style = "-fx-background-color: #4f5d75; -fx-text-fill: white;"
          onAction = _ => onRules()
        },
        new Button("Statistiche") {
          minWidth = 200
          font = Font.font("Arial", FontWeight.Bold, 18)
          style = "-fx-background-color: #2d3142; -fx-text-fill: white;"
          onAction = _ => onStats()
        }
      )
    }

    val menuBox = new VBox {
      spacing = 40
      alignment = Pos.Center
      children = Seq(
        title,
        buttonBox
      )
    }

    new StackPane {
      alignment = Pos.Center
      style = "-fx-background-color: linear-gradient(to bottom, #bfc0c0, #ffffff);"
      children = Seq(menuBox)
    }
  }
}
