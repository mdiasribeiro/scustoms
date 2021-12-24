package com.scustoms.trueskill

import com.scustoms.services.MatchmakingService.MatchTeam
import com.scustoms.trueskill.TrueskillUtils.square
import de.gesundkrank.jskills.trueskill.{DrawMargin, TruncatedGaussianCorrectionFunctions}
import de.gesundkrank.jskills.{GameInfo, Rating}

object TwoTeamCalculator {
  private val TotalPlayers = 10

  def calculatePlayerRatings(gameInfo: GameInfo, team1: MatchTeam, team2: MatchTeam, team1Won: Boolean
                            ): (MatchTeam, MatchTeam) = {
    val drawMargin: Double = DrawMargin.GetDrawMarginFromDrawProbability(gameInfo.getDrawProbability, gameInfo.getBeta)
    val betaSquared: Double = square(gameInfo.getBeta)
    val tauSquared: Double = square(gameInfo.getDynamicsFactor)

    val team1Ratings = team1.ratings
    val team2Ratings = team2.ratings

    val team1MeanSum: Double = team1Ratings.map(_.getMean).sum
    val team2MeanSum: Double = team2Ratings.map(_.getMean).sum
    val sum = team1Ratings.map(r => square(r.getStandardDeviation)).sum + team2Ratings.map(r => square(r.getStandardDeviation)).sum

    val c: Double = Math.sqrt(sum + TotalPlayers * betaSquared)

    val (winningMean, losingMean) = if (team1Won) (team1MeanSum, team2MeanSum) else (team2MeanSum, team1MeanSum)

    val meanDelta: Double = winningMean - losingMean

    val v = TruncatedGaussianCorrectionFunctions.vExceedsMargin(meanDelta, drawMargin, c)
    val w = TruncatedGaussianCorrectionFunctions.wExceedsMargin(meanDelta, drawMargin, c)
    val rankMultiplier = if (team1Won) 1 else -1

    def calculateNewRating(previousPlayerRating: Rating): Rating = {
      val meanMultiplier = (square(previousPlayerRating.getStandardDeviation) + tauSquared) / c
      val stdDevMultiplier = (square(previousPlayerRating.getStandardDeviation) + tauSquared) / square(c)

      val playerMeanDelta = rankMultiplier * meanMultiplier * v
      val newMean = previousPlayerRating.getMean + playerMeanDelta

      val newStdDev = math.sqrt((square(previousPlayerRating.getStandardDeviation) + tauSquared) * (1 - (w * stdDevMultiplier)))
      new Rating(newMean, newStdDev)
    }

    val team1NewRatings = team1Ratings.map(calculateNewRating)
    val team2NewRatings = team2Ratings.map(calculateNewRating)
    (team1.updatedRatings(team1NewRatings), team2.updatedRatings(team2NewRatings))
  }

  def calculateMatchQuality(gameInfo: GameInfo, team1: MatchTeam, team2: MatchTeam): Double = {
    val team1Ratings = team1.ratings
    val team2Ratings = team2.ratings

    val betaSquared = square(gameInfo.getBeta)

    val team1MeanSum: Double = team1Ratings.map(_.getMean).sum
    val team2MeanSum: Double = team2Ratings.map(_.getMean).sum

    val team1StdDevSquared: Double = team1Ratings.map(r => square(r.getStandardDeviation)).sum
    val team2SigmaSquared: Double = team2Ratings.map(r => square(r.getStandardDeviation)).sum

    val sqrtPart = math.sqrt(
      (TotalPlayers * betaSquared) / (TotalPlayers * betaSquared + team1StdDevSquared + team2SigmaSquared)
    )

    val expPart = math.exp(
      (-1 * square(team1MeanSum - team2MeanSum)) / (2 * (TotalPlayers * betaSquared + team1StdDevSquared + team2SigmaSquared))
    )

    expPart * sqrtPart
  }
}
