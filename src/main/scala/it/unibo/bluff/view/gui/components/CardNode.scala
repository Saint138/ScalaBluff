package it.unibo.bluff.view.gui.components

import it.unibo.bluff.model.{Card, Suit}
import scalafx.scene.layout.StackPane
import scalafx.geometry.Insets
import scalafx.scene.control.Label
import scalafx.scene.text.Font
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.paint.Color

object CardNode {
  private val baseStyle =
    "-fx-background-color: white; -fx-background-radius:12; -fx-border-radius:12;" +
      "-fx-border-color:#bfc0c0; -fx-effect:dropshadow(gaussian, rgba(0,0,0,0.10), 6, 0, 1, 1);"
  private val selectedStyle =
    "-fx-background-color: white; -fx-background-radius:12; -fx-border-radius:12;" +
      "-fx-border-color:#ef8354; -fx-border-width:3; -fx-effect:dropshadow(gaussian, rgba(239,131,84,0.35), 10, 0, 0, 0);"

  private def suitSymbol(s: Suit): String = s.toString.toLowerCase match
    case "cuori" | "hearts"   => "♥"
    case "quadri" | "diamonds"=> "♦"
    case "fiori"  | "clubs"   => "♣"
    case "picche" | "spades"  => "♠"
    case _                    => s.toString

  private def suitColor(s: Suit): Color = s.toString.toLowerCase match
    case "cuori" | "hearts" | "quadri" | "diamonds" => Color.web("#ef8354")
    case _                                          => Color.web("#2d3142")

  private def imagePath(card: Card): String =
    val rankStr = card.rank.toString.toLowerCase
    val suitStr = card.suit.toString.toLowerCase
    s"/cards/${rankStr}_${suitStr}.jpeg"

  def apply(card: Card, toggle: CardNode => Unit): CardNode =
    new CardNode(card, toggle)

  final class CardNode(val card: Card, toggle: CardNode => Unit) extends StackPane {
    minWidth = 82; prefWidth = 82; maxWidth = 82
    minHeight = 116; prefHeight = 116; maxHeight = 116
    padding = Insets(4)
    style = baseStyle

    private val maybeUrl = Option(getClass.getResource(imagePath(card)))
    children = maybeUrl match
      case Some(url) =>
        new ImageView(new Image(url.toExternalForm)) {
          fitWidth = 72; fitHeight = 108; preserveRatio = true
        }
      case None =>
        new Label(s"${card.rank} ${suitSymbol(card.suit)}") {
          font = Font.font("System", 14)
          textFill = suitColor(card.suit)
        }

    def markSelected(on: Boolean): Unit =
      style = if on then selectedStyle else baseStyle

    onMouseClicked = _ => toggle(this)
  }
}
