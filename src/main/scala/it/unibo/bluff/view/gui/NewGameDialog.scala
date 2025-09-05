package it.unibo.bluff.view.gui

import scalafx.Includes.*
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.geometry.Insets

/** Dialog iniziale: chiede numero giocatori, nomi e numero di round.
  * Ritorna (nomi, round) — round=1 => partita singola; >1 => torneo.
  */
object NewGameDialog {
  def askPlayers(): Option[(Vector[String], Int)] = {
    val dialog = new Dialog[ButtonType]() {
      title = "Nuova partita"
      headerText = "Imposta numero giocatori, nomi e round"
    }
    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)

    val spinnerPlayers = new Spinner[Int](2, 4, 2)
    val spinnerRounds  = new Spinner[Int](1, 20, 1)

    val nameFields = Vector.fill(4)(new TextField { promptText = "Nome" })
    nameFields.drop(2).foreach(_.disable = true)

    spinnerPlayers.valueProperty.onChange { (_, _, n) =>
      nameFields.zipWithIndex.foreach { case (tf, i) => tf.disable = i >= n }
    }

    val grid = new GridPane {
      hgap = 10; vgap = 8; padding = Insets(10)
      add(new Label("Giocatori:"), 0, 0); add(spinnerPlayers, 1, 0)
      (0 until 4).foreach { i =>
        add(new Label(s"Giocatore ${i+1}:"), 0, i+1); add(nameFields(i), 1, i+1)
      }
      add(new Separator(), 0, 5, 2, 1)
      add(new Label("Round (1 = singola partita):"), 0, 6); add(spinnerRounds, 1, 6)
    }
    dialog.dialogPane().content = grid

    dialog.resultConverter = (btn: ButtonType) => btn
    dialog.showAndWait().filter(_ == ButtonType.OK).map { _ =>
      val n = spinnerPlayers.value.value
      val names = nameFields.take(n).zipWithIndex.map { case (tf, i) =>
        Option(tf.text.value).map(_.trim).filter(_.nonEmpty).getOrElse(s"Player${i+1}")
      }.toVector
      val rounds = spinnerRounds.value.value
      (names, rounds)
    }
  }
}
