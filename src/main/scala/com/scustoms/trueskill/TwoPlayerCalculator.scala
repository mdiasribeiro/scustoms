package com.scustoms.trueskill

import com.scustoms.services.MatchService.MatchPlayer
import com.scustoms.trueskill.TrueskillUtils.square
import de.gesundkrank.jskills.trueskill.{DrawMargin, TruncatedGaussianCorrectionFunctions}
import de.gesundkrank.jskills.{GameInfo, Rating}

object TwoPlayerCalculator {

  def calculateNewRating(gameInfo: GameInfo, player1: MatchPlayer, player2: MatchPlayer, player1Won: Boolean
                        ): (MatchPlayer, MatchPlayer) = {
    val drawMargin = DrawMargin.GetDrawMarginFromDrawProbability(gameInfo.getDrawProbability, gameInfo.getBeta)

    val player1Rating = player1.getMatchRating
    val player2Rating = player2.getMatchRating

    val c = math.sqrt(
      square(player1Rating.getStandardDeviation) + square(player2Rating.getStandardDeviation) + 2 * square(gameInfo.getBeta)
    )

    val (winningMean, losingMean) = if (player1Won)
      (player1Rating.getMean, player2Rating.getMean)
    else
      (player2Rating.getMean, player1Rating.getMean)

    val meanDelta = winningMean - losingMean

    val v = TruncatedGaussianCorrectionFunctions.vExceedsMargin(meanDelta, drawMargin, c)
    val w = TruncatedGaussianCorrectionFunctions.wExceedsMargin(meanDelta, drawMargin, c)

    def calculateNewRating(previousPlayerRating: Rating, won: Boolean): Rating = {
      val meanMultiplier = (square(previousPlayerRating.getStandardDeviation) + square(gameInfo.getDynamicsFactor)) / c

      val varianceWithDynamics = square(previousPlayerRating.getStandardDeviation) + square(gameInfo.getDynamicsFactor)
      val stdDevMultiplier = varianceWithDynamics / square(c)

      val rankMultiplier = if (won) 1 else -1
      val newMean = previousPlayerRating.getMean + (rankMultiplier * meanMultiplier * v)
      val newStdDev = math.sqrt(varianceWithDynamics * (1 - w * stdDevMultiplier))

      new Rating(newMean, newStdDev)
    }

    val updatedPlayer1 = player1.updatedRating(calculateNewRating(player1Rating, player1Won), player1Won)
    val updatedPlayer2 = player2.updatedRating(calculateNewRating(player2Rating, !player1Won), !player1Won)
    (updatedPlayer1, updatedPlayer2)
  }

  def calculateLaneQuality(gameInfo: GameInfo, player1: MatchPlayer, player2: MatchPlayer): Double = {
    val player1Rating = player1.getMatchRating
    val player2Rating = player2.getMatchRating

    val betaSquared = square(gameInfo.getBeta)
    val player1SigmaSquared = square(player1Rating.getStandardDeviation)
    val player2SigmaSquared = square(player2Rating.getStandardDeviation)

    val sqrtPart = math.sqrt((2 * betaSquared) / (2 * betaSquared + player1SigmaSquared + player2SigmaSquared))

    val expPart = math.exp(
      (-1 * square(player1Rating.getMean - player2Rating.getMean)) / (2 * (2 * betaSquared + player1SigmaSquared + player2SigmaSquared))
    )

    sqrtPart * expPart
  }
}
