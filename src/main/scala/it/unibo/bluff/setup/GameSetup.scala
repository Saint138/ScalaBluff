package it.unibo.bluff.setup

import it.unibo.bluff.engine.Engine
import it.unibo.bluff.engine.Engine.{GameCommand, GameEvent}
import it.unibo.bluff.model.*
import it.unibo.bluff.model.state.GameState
import it.unibo.bluff.model.TurnOrder.given
//game setup for gui section , to be improved if necessary..
object GameSetup {

  def fairInitialDeal(numPlayers: Int, names: Vector[String]): (GameState, List[GameEvent], Int) = {
    val MaxAttempts = 100
    var attempt = 0
    var lastGood: Option[(GameState, List[GameEvent], Int)] = None

    while (attempt < MaxAttempts) {
      val shuffler = Shuffler.random
      val deckObj  = Dealing.initialDeckForPlayers(numPlayers, shuffler)
      val deck     = deckObj match { case ListDeck(cs) => cs }

      val st0 = GameState.initial(players = numPlayers, playerNames = names, shuffled = deck)

      Engine.step(st0, GameCommand.Deal) match {
        case Right((st1, evs)) =>
          if (!hasAnyQuartet(st1)) return (st1, evs, deck.size)
          else if (lastGood.isEmpty) lastGood = Some((st1, evs, deck.size))
        case Left(_) => ()
      }
      attempt += 1
    }
    lastGood.get
  }

  private def hasAnyQuartet(st: GameState): Boolean =
    st.hands.values.exists(h => h.cards.groupBy(_.rank).values.exists(_.size == 4))
}
