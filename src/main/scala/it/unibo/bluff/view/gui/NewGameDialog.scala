package it.unibo.bluff.view.gui

import scalafx.Includes.*
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.geometry.Insets
//initial set up dialog for game when you press the start game button.
object NewGameDialog {
  def askPlayers(): Option[Vector[String]] = {
    val dialog = new Dialog[ButtonType]() {
      title = "Nuova partita"
      headerText = "Imposta numero giocatori e nomi"
    }
    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)

    val spinner    = new Spinner[Int](2, 4, 2)
    val nameFields = Vector.fill(4)(new TextField { promptText = "Nome" })
    nameFields.drop(2).foreach(_.disable = true)
    spinner.valueProperty.onChange { (_, _, n) =>
      nameFields.zipWithIndex.foreach { case (tf, i) => tf.disable = i >= n }
    }

    val grid = new GridPane {
      hgap = 10; vgap = 8; padding = Insets(10)
      add(new Label("Giocatori:"), 0, 0); add(spinner, 1, 0)
      (0 until 4).foreach { i =>
        add(new Label(s"Giocatore ${i+1}:"), 0, i+1); add(nameFields(i), 1, i+1)
      }
    }
    dialog.dialogPane().content = grid

    dialog.resultConverter = (btn: ButtonType) => btn
    dialog.showAndWait().filter(_ == ButtonType.OK).map { _ =>
      val n = spinner.value.value
      nameFields.take(n).zipWithIndex.map { case (tf, i) =>
        Option(tf.text.value).map(_.trim).filter(_.nonEmpty).getOrElse(s"Player${i+1}")
      }.toVector
    }
  }
}
