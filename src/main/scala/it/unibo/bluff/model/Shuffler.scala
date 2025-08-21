package it.unibo.bluff.model

import scala.util.Random

trait Shuffler:
  def shuffle(cards: List[Card]): List[Card]

object Shuffler:
  /** Deterministic shuffler from a fixed seed (reproducible). */
  def deterministic(seed: Long): Shuffler = new Shuffler:
    def shuffle(cards: List[Card]): List[Card] =
      // Fisher-Yates in a mutable buffer, then to List (deterministic given seed)
      val rnd = new Random(seed)
      val buf = scala.collection.mutable.ArrayBuffer.from(cards)
      var i = buf.length - 1
      while i > 0 do
        val j = rnd.nextInt(i + 1)
        val tmp = buf(i); buf(i) = buf(j); buf(j) = tmp
        i -= 1
      buf.toList

  /** Non-deterministic (fresh Random). */
  val random: Shuffler = new Shuffler:
    def shuffle(cards: List[Card]): List[Card] =
      Random.shuffle(cards)
