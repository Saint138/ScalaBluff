package it.unibo.bluff.view.gui

import scalafx.stage.Stage
import scalafx.scene.Scene
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.scene.control.{Button, TextArea}
import scalafx.geometry.{Insets, Pos}
import scalafx.Includes.*

object RulesDialog:
  def show(onBack: () => Unit = () => ()): Unit =
    val rulesText =
      """
      Dubito (Bluff) - Regole implementate in ScalaBluff

      Modalità di gioco:
      - VS Bot: un solo giocatore umano contro un bot che gioca automaticamente.
      - Multiplayer: più giocatori umani locali (turni passati in ordine).

      Obiettivo:
      - Svuotare la propria mano. Il primo che non ha più carte vince il round.

      Turno e dichiarazioni:
      - Il giocatore attivo dichiara un rango (es. Due, Tre, Asso, ...) e mette da 1 a N carte coperte nella pila centrale.
      - La dichiarazione può essere vera o falsa (bluff).

      Chiamare bluff:
      - Qualsiasi altro giocatore può chiamare "Accusa Bluff" sull'ultima dichiarazione.
      - Se la dichiarazione era falsa, il dichiarante subisce la penalità (prende la pila o altro previsto dalla logica).
      - Se la dichiarazione era vera, chi ha chiamato il bluff subisce la penalità.

      Penalità e avanzamento:
      - Le penalità sono gestite dal motore di gioco (Engine) e si traducono in carte prese dalla pila o aggiornamenti di stato.
      - Dopo la risoluzione, il turno prosegue secondo la logica implementata.

      Timer e bot:
      - Ogni giocatore ha un timer per turno; se scade si verifica un Timeout e si applicano le conseguenze.
      - In VS Bot il bot gioca automaticamente (le azioni del bot sono visibili nel log e in console).

      Note UI:
      - Seleziona le carte dalla tua mano, scegli il rango e premi "Gioca".
      - Puoi chiamare "Accusa Bluff" dopo la dichiarazione di un avversario.
      - L'area log mostra gli eventi di gioco; le azioni del bot vengono riportate anche nella console per debug.
      """.stripMargin

    val textArea = new TextArea {
      editable = false
      wrapText = true
      text = rulesText
      prefColumnCount = 60
      prefRowCount = 22
    }

    val closeBtn = Button("Close")

    val buttons = new HBox {
      spacing = 10
      children = Seq(closeBtn)
      alignment = Pos.Center
      padding = Insets(8)
    }

    val mainPane = new BorderPane {
      center = textArea
      bottom = buttons
      padding = Insets(10)
    }

    val dlgStage = new Stage {
      title = "Regole di Dubito"
      scene = new Scene(800, 520) {
        root = mainPane
      }
    }
    closeBtn.onAction = _ => {
      try onBack()
      finally dlgStage.close()
    }

    dlgStage.show()
