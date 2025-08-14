import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.view.cli.CLI

class CLITest extends AnyFunSuite {
  test("new should initialize a fresh game") {
    CLI.execute("new") //creo una partita con due giocatori
    assert(CLI.gameState.isDefined)
    assert(CLI.gameState.get.players.nonEmpty)
  }

  test("status should show current player turn"){
    CLI.execute("new")
    val output = CLI.execute("status")
    assert(output.contains("Turno attuale:"))
  }
}