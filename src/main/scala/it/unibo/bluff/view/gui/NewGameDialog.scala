package it.unibo.bluff.view.gui

import scalafx.Includes.*
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.geometry.Insets

/** Dialog iniziale: chiede numero giocatori, nomi, round e tipo di bot.
 * Ritorna (isSinglePlayerMode, nomi, round, botKind).
 */
object NewGameDialog {

  /** Returns (isSinglePlayerMode, names, rounds, botKind) */
  def askPlayers(): Option[(Boolean, Vector[String], Int, String)] = {
    val dialog = new Dialog[ButtonType]() {
      title = "Nuova partita"
      headerText = "Imposta modalità, nomi e round"
    }
    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)

    // --- Controlli modalità
    val spinnerPlayers = new Spinner[Int](2, 4, 2)
    val spinnerRounds  = new Spinner[Int](1, 20, 1)

    val rbSingle = new RadioButton("Partita contro Bot") { selected = true }
    val rbMulti  = new RadioButton("Multigiocatore")
    val tg = new ToggleGroup()
    rbSingle.toggleGroup = tg
    rbMulti.toggleGroup  = tg

    // --- Scelta bot
    val botChoice = new ComboBox[String](Seq("facile","medio", "difficile")) {
      value = "facile"
    }

    // --- Nomi giocatori
    val nameFields: Vector[TextField] =
      Vector.fill(4)(new TextField { promptText = "Nome" })
    nameFields.drop(2).foreach(_.disable = true)

    spinnerPlayers.valueProperty.onChange { (_, _, n) =>
      nameFields.zipWithIndex.foreach { case (tf, i) =>
        tf.disable = i >= n
      }
    }

    // --- Grid per multiplayer
    val grid = new GridPane {
      hgap = 10
      vgap = 8
      padding = Insets(10)

      add(new Label("Giocatori:"), 0, 0)
      add(spinnerPlayers,           1, 0)

      (0 until 4).foreach { i =>
        add(new Label(s"Giocatore ${i + 1}:"), 0, i + 1)
        add(nameFields(i),                    1, i + 1)
      }

      add(new Separator(), 0, 5, 2, 1)
      add(new Label("Round (1 = singola partita):"), 0, 6)
      add(spinnerRounds,                               1, 6)
    }

    // --- Single player
    val singleName = new TextField {
      promptText = "Nome giocatore"
      text = "Player1"
    }

    val vbox = new VBox(8,
      rbSingle,
      new HBox(6, new Label("Nome:"), singleName),
      new HBox(6, new Label("Tipo Bot:"), botChoice),
      rbMulti,
      grid
    )
    dialog.dialogPane().content = vbox

    // --- Abilitazioni dinamiche
    singleName.disable = false
    botChoice.disable = false
    grid.disable = true

    rbSingle.selected.onChange { (_, _, sel) =>
      grid.disable = sel
      singleName.disable = !sel
      botChoice.disable = !sel
    }
    rbMulti.selected.onChange { (_, _, sel) =>
      grid.disable = !sel
      singleName.disable = sel
      botChoice.disable = sel
    }

    dialog.resultConverter = (btn: ButtonType) => btn

    dialog.showAndWait()
      .filter(_ == ButtonType.OK)
      .map { _ =>
        val rounds = spinnerRounds.value.value
        if (rbSingle.selected.value) {
          val playerName =
            Option(singleName.text.value).map(_.trim).filter(_.nonEmpty).getOrElse("Player1")
          val kind = botChoice.value.value
          (true, Vector(playerName, "Bot"), rounds, kind)
        } else {
          val n = spinnerPlayers.value.value
          val names =
            nameFields.take(n).zipWithIndex.map { case (tf, i) =>
              Option(tf.text.value).map(_.trim).filter(_.nonEmpty).getOrElse(s"Player${i + 1}")
            }.toVector
          (false, names, rounds, "random") // botKind irrilevante in multi
        }
      }
  }
}
