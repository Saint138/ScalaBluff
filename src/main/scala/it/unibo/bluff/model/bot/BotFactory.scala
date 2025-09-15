package it.unibo.bluff.model.bot

import it.unibo.bluff.model.PlayerId

object BotFactory:
  def apply(kind: String, id: PlayerId): Bot = kind.toLowerCase match
    case "facile"  => RandomBot(id)
    case "medio" => StrategicBot(id)
    case "difficile" => SmartBot(id)
    case other    =>
      println(s"[WARN] Bot kind '$other' non riconosciuto, uso RandomBot.")
      RandomBot(id)
