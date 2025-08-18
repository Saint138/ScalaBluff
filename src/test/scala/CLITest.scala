import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.view.cli.CLI

class CLITest extends AnyFunSuite {
  test("new should initialize a fresh game") {
    val output = CLI.execute("new") //creo una partita con due giocatori
    assert(CLI.gameState.isDefined)
    assert(CLI.gameState.get.players.nonEmpty)
    assert(output.contains("nuova partita"))
  }

  test("status should show current player turn"){
    CLI.execute("new")
    val output = CLI.execute("status")
    assert(output.contains("Turno attuale:"))
  }

  test("status should show current player ") {
    val output = CLI.execute("status")
    assert(output.contains("Nessuna partita in corso"))
  }

  test("end-turn should pass the turn to the next player"){
    CLI.execute("new")
    val before = CLI.execute("status")
    val output = CLI.execute("end-turn")
    val after = CLI.execute("status")
    assert(output.contains("Cambio turno"))
    assert(before != after)

  }
}