package it.unibo.bluff.model.util

import it.unibo.bluff.model.cards.Card

trait RNG:
  def nextInt(bound: Int): Int
  def shuffle[A](xs: List[A]): List[A] =
    val buf = scala.collection.mutable.ArrayBuffer.from(xs)
    val out = scala.collection.mutable.ArrayBuffer[A]()
    while buf.nonEmpty do
      val i = nextInt(buf.size)
      out += buf.remove(i)
    out.toList
object RNG:
  def default(seed: Long = System.currentTimeMillis()): RNG = new RNG:
    private val rnd = new scala.util.Random(seed)
    def nextInt(bound: Int): Int = rnd.nextInt(bound)
