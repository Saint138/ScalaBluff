package it.unibo.bluff.view.cli

import org.scalatest.funsuite.AnyFunSuite
import it.unibo.bluff.view.cli.CLI
import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.GameCommand

class CLITest extends AnyFunSuite {

  val NumPlayers = 3

//  test("new should initialize a fresh game") {
//    CLI.start(NumPlayers)
//    val st = CLI.currentState.get
//    assert(st.turn == st.players.head)
//  }
//
//  test("status should reflect current game state") {
//    CLI.start(NumPlayers)
//    val st = CLI.currentState.get
//    assert(st.turn == st.players.head)
//  }
//
//  test("simulate simple game turns") {
//    CLI.start(NumPlayers)
//    val st1 = CLI.currentState.get
//    assert(st1.turn == st1.players.head)
//
//    CLI.execute("deal")
//
//    val afterDeal = CLI.currentState.get
//    assert(afterDeal.hands.size == NumPlayers)
//    assert(afterDeal.turn == st1.players.head)
//  }
//
//  test("turn passes to next player after a play") {
//    CLI.start(NumPlayers)
//    CLI.execute("deal")
//    val stBefore = CLI.currentState.get
//    val currentPlayer = stBefore.turn
//    val cardToPlay = stBefore.hands(currentPlayer).cards.head
//
//    CLI.execute(s"play ${cardToPlay.rank.toString.toLowerCase} 1")
//    val stAfter = CLI.currentState.get
//
//    assert(stAfter != stBefore, "Il turno è passato al giocatore successivo")
//  }


}
