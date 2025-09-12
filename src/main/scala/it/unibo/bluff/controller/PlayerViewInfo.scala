package it.unibo.bluff.controller

import it.unibo.bluff.model.*

final case class PlayerViewInfo(
                                 playerName: String,
                                 handSize: Int,
                                 isCurrentTurn: Boolean,
                                 timeRemainingMs: Long
                               )

final case class GameViewInfo(
                               players: Vector[PlayerViewInfo],
                               currentPlayerName: String,
                               canPlay: Boolean,
                               playedCardsLog: List[String] // log testuale degli eventi
                             )
