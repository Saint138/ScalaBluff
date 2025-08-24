package it.unibo.bluff

import it.unibo.bluff.view.cli.CLI

object Main:
  def main(args: Array[String]): Unit =
    CLI.repl()
