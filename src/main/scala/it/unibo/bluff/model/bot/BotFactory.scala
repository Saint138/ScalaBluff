package it.unibo.bluff.model.bot

import it.unibo.bluff.model.PlayerId

object BotFactory:
  def apply(kind: String, id: PlayerId): Bot = kind.toLowerCase match
    case "smart"  => SmartBot(id)
    case "random" => RandomBot(id)
    case other    =>
      println(s"[WARN] Bot kind '$other' non riconosciuto, uso RandomBot.")
      RandomBot(id)
