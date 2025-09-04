package gui

import java.util.concurrent.atomic.AtomicReference
import it.unibo.bluff.view.gui.{MainMenuView, NewGameDialog, GameView}
import it.unibo.bluff.setup.GameSetup
import it.unibo.bluff.timer.GameTimer
import it.unibo.bluff.model.bot.{RandomBot, BotRunner}
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand
import it.unibo.bluff.model.TurnOrder.given
import it.unibo.bluff.model.state.{GameState, GameClocks}
import it.unibo.bluff.model.PlayerId
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.stage.StageStyle

object MainGUI extends JFXApp3:

  private val stateRef = new AtomicReference[GameState]()
  private var timer: Option[GameTimer] = None
  private var botRunner: Option[BotRunner] = None

  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      initStyle(StageStyle.Decorated)
      title = "ScalaBluff"
      width = 800
      height = 600
      scene = new Scene(width.value, height.value):
        root = MainMenuView(
          onNewGame = () =>
            NewGameDialog.askPlayers().foreach { case (isSingle, names) =>
              val playerNames =
                if isSingle then names 
                else names
              val (stDealt, _, _) = GameSetup.fairInitialDeal(playerNames.size, playerNames)
              val stWithClocks = GameClocks.withClocks(stDealt, 60_000L)
              stateRef.set(stWithClocks)

              // Avvia la vista di gioco (creiamo la view prima così possiamo trovare il TextArea per i log)
              val gamePane = GameView(stateRef)
              // try to find the first TextArea under gamePane to use as log target
              def findTA(p: javafx.scene.Parent): Option[scalafx.scene.control.TextArea] =
                import scala.jdk.CollectionConverters.*
                val it = p.getChildrenUnmodifiable.iterator()
                while it.hasNext do
                  val ch = it.next()
                  ch match
                    case ta: javafx.scene.control.TextArea => return Some(new scalafx.scene.control.TextArea(ta))
                    case par: javafx.scene.Parent =>
                      val r = findTA(par.asInstanceOf[javafx.scene.Parent])
                      if r.isDefined then return r
                    case _ => ()
                None

              // GameView returns a ScalaFX BorderPane; get its underlying JavaFX node via delegate
              val maybeTA =
                gamePane.delegate match
                  case p: javafx.scene.Parent => findTA(p)
                  case _ => None

              // se è una partita contro bot, avvia il BotRunner e passa la callback
              if isSingle then
                val bot = RandomBot(it.unibo.bluff.model.PlayerId(1))
                botRunner.foreach(_.stop())
                val cb: Option[String => Unit] = maybeTA.map(ta => (s: String) => scalafx.application.Platform.runLater { ta.appendText(if s.endsWith("\n") then s else s + "\n") })
                val br = new BotRunner(stateRef, bot)
                botRunner = Some(br)
                br.start()

              // Timer per la partita
              val t = new GameTimer(stateRef, tickMillis = 200L, onTimeout = { pid =>
                given it.unibo.bluff.model.TurnOrder = summon[it.unibo.bluff.model.TurnOrder]
                Engine.step(stateRef.get(), GameCommand.Timeout(pid)) match
                  case Left(err) => println(s"Timeout ignored: $err")
                  case Right((newState, _)) =>
                    stateRef.set(newState)
                    val playerName = stateRef.get().nameOf(pid)
                    println(s"TimerExpired for $playerName")
              })
              timer.foreach(_.stop())
              timer = Some(t)
              t.start()

              // Avvia la vista di gioco
              root = new BorderPane:
                center = gamePane
            },
          onRules = () => println("Mostra regole..."),
          onStats = () => println("Mostra statistiche...")
        )

    stage.centerOnScreen()
