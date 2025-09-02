package gui

import java.util.concurrent.atomic.AtomicReference

import it.unibo.bluff.view.gui.{MainMenuView, NewGameDialog, GameView}
import it.unibo.bluff.setup.GameSetup
import it.unibo.bluff.timer.GameTimer
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.state.GameState
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.stage.StageStyle

object MainGUI extends JFXApp3:
  private val stateRef = new AtomicReference[GameState]()
  private var timer: Option[GameTimer] = None

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 800
      height = 600
      scene = new Scene(width.value, height.value):
        root = MainMenuView(
          onNewGame = () =>
            NewGameDialog.askPlayers().foreach { names =>
              val (stDealt, _, _) = GameSetup.fairInitialDeal(names.size, names)
              // inizializza clocks con 60_000 ms per giocatore
              val stWithClocks = stDealt.withClocks(60_000L)
              stateRef.set(stWithClocks)
              // crea e avvia timer
              val t = new GameTimer(stateRef, tickMillis = 200L, onTimeout = { pid =>
                given it.unibo.bluff.model.TurnOrder = summon[ it.unibo.bluff.model.TurnOrder ]
                Engine.step(stateRef.get(), GameCommand.Timeout(pid)) match
                  case Left(err) => println(s"Timeout ignored: $err")
                  case Right((newState, evs)) =>
                    stateRef.set(newState)
                    println(s"TimerExpired for ${pid.value}, events: $evs")
              })
              timer.foreach(_.stop())
              timer = Some(t)
              t.start()

              root = new BorderPane:
                center = it.unibo.bluff.view.gui.GameView(stateRef)
            },
          onRules = () => println("Mostra regole..."),
          onStats = () => println("Mostra statistiche...")
        )

    stage.centerOnScreen()
