package it.unibo.bluff.view.gui

import scalafx.Includes.*
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.geometry.Insets

object NewGameDialog {

  /** Mostra la finestra per scegliere modalità di gioco e nomi */
  def askPlayers(): Option[(Boolean, Vector[String])] =
    val dialog = new Dialog[ButtonType]():
      title = "Nuova partita"
      headerText = "Scegli modalità di gioco"

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)

    // Radio per modalità singolo vs bot o multiplayer
    val rbSingle = new RadioButton("Partita contro Bot") { selected = true }
    val rbMulti  = new RadioButton("Multigiocatore")
    val toggleGroup = new ToggleGroup()
    rbSingle.toggleGroup = toggleGroup
    rbMulti.toggleGroup  = toggleGroup

    // Componenti multiplayer
    val spinner = new Spinner[Int](2, 4, 2)
    val nameFields = Vector.fill(4)(new TextField { promptText = "Nome giocatore" })
    nameFields.drop(2).foreach(_.disable = true)

    spinner.valueProperty.onChange { (_, _, n) =>
      nameFields.zipWithIndex.foreach { case (tf, i) => tf.disable = i >= n }
    }

    val multiGrid = new GridPane {
      hgap = 10; vgap = 8; padding = Insets(10)
      add(new Label("Numero giocatori:"), 0, 0); add(spinner, 1, 0)
      (0 until 4).foreach { i =>
        add(new Label(s"Giocatore ${i + 1}:"), 0, i + 1); add(nameFields(i), 1, i + 1)
      }
    }

    // Layout
    val vbox = new VBox(10, rbSingle, rbMulti, multiGrid)
    dialog.dialogPane().content = vbox

    // Mostra/nasconde il pannello multiplayer
    rbSingle.selected.onChange { (_, _, sel) => multiGrid.disable = sel }
    rbMulti.selected.onChange  { (_, _, sel) => multiGrid.disable = !sel }

    dialog.resultConverter = (btn: ButtonType) => btn

    dialog.showAndWait().filter(_ == ButtonType.OK).map { _ =>
      if rbSingle.selected.value then
        (true, Vector("Player1", "Bot"))
      else
        val n = spinner.value.value
        val names = nameFields.take(n).zipWithIndex.map { case (tf, i) =>
          Option(tf.text.value).map(_.trim).filter(_.nonEmpty).getOrElse(s"Player${i + 1}")
        }.toVector
        (false, names)
    }
}
