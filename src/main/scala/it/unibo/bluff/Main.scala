package it.unibo.bluff

import it.unibo.bluff.view.cli.CLI

object Main:
  def main(args: Array[String]): Unit =
    CLI.start(players = 2)
    CLI.repl()
