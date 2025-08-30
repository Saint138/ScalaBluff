package it.unibo.bluff.view.gui

import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{StackPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

object MainMenuView {
  def apply(
    onNewGame: () => Unit,
    onRules:   () => Unit,
    onStats:   () => Unit
  ): StackPane = {
    val title = new Label("ScalaBluff") {
      font = Font.font("Verdana", FontWeight.Bold, 40)
      textFill = Color.web("#2d3142")
      alignment = Pos.Center
      style = "-fx-effect: dropshadow(gaussian, #bfc0c0, 8, 0, 2, 2);"
      maxWidth = Double.MaxValue
    }
    val btnNew = new Button("Nuovo gioco") {
      maxWidth = Double.MaxValue
      prefWidth = 320
      font = Font.font("Arial", FontWeight.Bold, 18)
      style = "-fx-background-color: #ef8354; -fx-text-fill: white;"
      onAction = _ => onNewGame()
    }
    val btnRules = new Button("Regole") {
      maxWidth = Double.MaxValue
      prefWidth = 320
      font = Font.font("Arial", FontWeight.Bold, 18)
      style = "-fx-background-color: #4f5d75; -fx-text-fill: white;"
      onAction = _ => onRules()
    }
    val btnStats = new Button("Statistiche") {
      maxWidth = Double.MaxValue
      prefWidth = 320
      font = Font.font("Arial", FontWeight.Bold, 18)
      style = "-fx-background-color: #2d3142; -fx-text-fill: white;"
      onAction = _ => onStats()
    }
    val buttonBox = new VBox {
      spacing = 15
      alignment = Pos.Center
      fillWidth = true
      maxWidth = 400
      children = Seq(btnNew, btnRules, btnStats)
    }
    val menuBox = new VBox {
      spacing = 40
      alignment = Pos.Center
      children = Seq(title, buttonBox)
      maxWidth = Double.MaxValue
      maxHeight = Double.MaxValue
    }
    new StackPane {
      alignment = Pos.Center
      style = "-fx-background-color: linear-gradient(to bottom, #bfc0c0, #ffffff);"
      children = Seq(menuBox)
      maxWidth = Double.MaxValue
      maxHeight = Double.MaxValue
    }
  }
}
