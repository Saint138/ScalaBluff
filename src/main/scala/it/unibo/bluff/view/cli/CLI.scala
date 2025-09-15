package it.unibo.bluff.view.cli

/** Entry point della CLI */
object CLI:
  private val controller = new CLICommandHandler()

  def repl(): Unit = controller.repl()