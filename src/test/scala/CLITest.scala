package it.unibo.bluff.view.cli

import org.scalatest.funsuite.AnyFunSuite

class CLITest extends AnyFunSuite {

  test("new should initialize a fresh game") {
    val _ = CLI.execute("new") // creo una partita con due giocatori
    assert(CLI.state.isDefined)
    assert(CLI.state.get.players.nonEmpty)
  }

  test("status should reflect current game state") {
    CLI.execute("new")
    val msg = CLI.statusMessage
    assert(msg.contains(s"Turno: ${CLI.state.get.turn.value}"))
  }

  test("end-turn should update current player") {
    CLI.execute("new")
    val before = CLI.state.get.turn
    CLI.execute("end-turn")
    val after = CLI.state.get.turn
    assert(before != after)
  }
}
