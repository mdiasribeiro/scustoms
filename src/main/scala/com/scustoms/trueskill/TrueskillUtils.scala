package com.scustoms.trueskill

object TrueskillUtils {
  def square(d: Double): Double = d * d

  def mean(collection: Seq[Double]): Double = collection.sum / collection.length
}
