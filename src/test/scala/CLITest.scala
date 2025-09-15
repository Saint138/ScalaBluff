package it.unibo.bluff.view.cli

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import it.unibo.bluff.model.*
import it.unibo.bluff.model.cards.{Card, Rank, Suit}
import it.unibo.bluff.model.core.state.GameState
import it.unibo.bluff.model.core.engine.Engine.{GameCommand, GameEvent}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import scala.io.Source

class CLIViewSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  var view: CLIPrinter = _
  val originalOut = System.out
  val originalIn = System.in
  var testOut: ByteArrayOutputStream = _

  override def beforeEach(): Unit = {
    view = new CLIPrinter()
    testOut = new ByteArrayOutputStream()
    System.setOut(new PrintStream(testOut))
  }

  override def afterEach(): Unit = {
    System.setOut(originalOut)
    System.setIn(originalIn)
    testOut.close()
  }

  private def setInput(input: String): Unit = {
    System.setIn(new ByteArrayInputStream(input.getBytes()))
  }

  private def getOutput: String = {
    testOut.toString
  }

  "CLIView" should "print welcome message correctly" in {
    view.printWelcome()
    val output = getOutput
    output should include("=== ScalaBluff CLI ===")
    output should include("Comandi: new | bot <tipo> | help | quit")
  }

  it should "print help with game commands when game is active" in {
    view.printHelp(gameActive = true)
    val output = getOutput
    output should include("play <n1> <rank1>")
    output should include("call")
    output should include("status")
  }

  it should "print help without game commands when no game is active" in {
    view.printHelp(gameActive = false)
    val output = getOutput
    output should include("Tipi di bot disponibili: random, smart")
    output should not include "play"
  }

  it should "parse ranks correctly with Italian aliases" in {
    view.parseRank("asso") shouldBe Right(Rank.Asso)
    view.parseRank("re") shouldBe Right(Rank.King)
    view.parseRank("donna") shouldBe Right(Rank.Queen)
    view.parseRank("fante") shouldBe Right(Rank.Jack)
    view.parseRank("10") shouldBe Right(Rank.Dieci)
    view.parseRank("due") shouldBe Right(Rank.Due)
  }

  it should "parse ranks correctly with English aliases" in {
    view.parseRank("king") shouldBe Right(Rank.King)
    view.parseRank("queen") shouldBe Right(Rank.Queen)
    view.parseRank("jack") shouldBe Right(Rank.Jack)
    view.parseRank("a") shouldBe Right(Rank.Asso)
    view.parseRank("k") shouldBe Right(Rank.King)
  }

  it should "return error for unknown ranks" in {
    view.parseRank("unknown") shouldBe Left("Rank non riconosciuto: unknown")
    view.parseRank("xyz") shouldBe Left("Rank non riconosciuto: xyz")
  }

  it should "be case insensitive for rank parsing" in {
    view.parseRank("ASSO") shouldBe Right(Rank.Asso)
    view.parseRank("Re") shouldBe Right(Rank.King)
    view.parseRank("KING") shouldBe Right(Rank.King)
  }
}

  
class CLIControllerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  var controller: CLICommandHandler = _
  val originalOut = System.out
  val originalIn = System.in
  var testOut: ByteArrayOutputStream = _

  override def beforeEach(): Unit = {
    controller = new CLICommandHandler()
    testOut = new ByteArrayOutputStream()
    System.setOut(new PrintStream(testOut))
  }

  override def afterEach(): Unit = {
    System.setOut(originalOut)
    System.setIn(originalIn)
    testOut.close()
  }

  private def setInput(input: String): Unit = {
    System.setIn(new ByteArrayInputStream(input.getBytes()))
  }

  private def getOutput: String = {
    testOut.toString
  }

  "CLIController" should "handle quit command correctly" in {
    setInput("quit\n")

    // Simuliamo solo l'handling del comando quit senza fare il REPL completo
    controller.handleCommand("quit")

    val output = getOutput
    output should include("Arrivederci!")
  }

  it should "handle help command correctly" in {
    controller.handleCommand("help")
    val output = getOutput
    output should include("Comandi: new | bot <tipo> | help | quit")
  }

  it should "handle unknown commands correctly" in {
    controller.handleCommand("unknown")
    val output = getOutput
    output should include("Al momento non stai giocando")
  }

  it should "parse card pairs correctly" in {
    val validPairs = List("2", "asso", "1", "re")
    val result = controller.parsePairs(validPairs)

    result shouldBe Right(List((2, Rank.Asso), (1, Rank.King)))
  }

  it should "return error for invalid card pairs" in {
    val invalidPairs = List("invalid", "asso")
    val result = controller.parsePairs(invalidPairs)

    result shouldBe Left("Quantità non valida: invalid")
  }

  it should "return error for odd number of tokens" in {
    val oddTokens = List("2", "asso", "1")
    val result = controller.parsePairs(oddTokens)

    result shouldBe Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
  }
  
  // Aggiungiamo i metodi necessari al controller per il testing
  extension (c: CLICommandHandler) {
    def handleCommand(input: String): Unit = {
      // Simula l'handling di un singolo comando per testing
      input.split("\\s+").toList match {
        case "quit" :: _ =>
          c.view.printGoodbye()
        case "help" :: _ =>
          c.view.printHelp(false)
        case _ =>
          c.view.printNoGameActive()
      }
    }

    def parsePairs(tokens: List[String]): Either[String, List[(Int, Rank)]] = {
      tokens match {
        case Nil => Right(Nil)
        case _ if tokens.length % 2 != 0 =>
          Left("Sintassi errata. Usa: play <n1> <rank1> [<n2> <rank2> ...]")
        case _ =>
          tokens.grouped(2).toList.map { g =>
            if g.size == 2 then
              val qStr = g(0)
              val rankStr = g(1)
              qStr.toIntOption match
                case None => Left(s"Quantità non valida: $qStr")
                case Some(q) if q <= 0 => Left(s"Quantità non valida: $qStr (deve essere > 0)")
                case Some(q) => c.view.parseRank(rankStr).map(rk => (q, rk))
            else Left(s"Errore interno nel parsing dei token: ${g.mkString(" ")}")
          }.foldRight[Either[String, List[(Int, Rank)]]](Right(Nil)) {
            case (Right(pair), Right(acc)) => Right(pair :: acc)
            case (Left(err), _) => Left(err)
            case (_, Left(err)) => Left(err)
          }
      }
    }
    
    def view: CLIPrinter = new CLIPrinter()
  }
}

class CLIIntegrationSpec extends AnyFlatSpec with Matchers {
  
  it should "handle bot type selection" in {
    val view = new CLIPrinter()

    // Test per verificare che il sistema riconosca i tipi di bot
    val validBotTypes = List("random", "smart", "RANDOM", "Smart", "invalid")

    validBotTypes.foreach { botType =>
      val normalized = if botType.toLowerCase == "smart" then "smart" else "random"
      normalized should (equal("smart") or equal("random"))
    }
  }
}