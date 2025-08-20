import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.view.cli.CLI

class CLITest extends AnyFunSuite {
  test("new should initialize a fresh game") {
    val output = CLI.execute("new") //creo una partita con due giocatori
    assert(CLI.gameState.isDefined)
    assert(CLI.gameState.get.players.nonEmpty)
  }

  test("status should reflect current game state") {
    CLI.execute("new")
    val msg = CLI.statusMessage
    assert(msg.contains(s"Turno attuale: ${CLI.gameState.get.turn}"))
  }

  test("end-turn should update current player") {
    CLI.execute("new")
    val before = CLI.gameState.get.turn
    CLI.execute("end-turn")
    val after = CLI.gameState.get.turn
    assert(before != after)
  }
}