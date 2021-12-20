package com.scustoms.database.keepers

import ackcord.data.UserId
import com.scustoms.database.DatabaseManager
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.services.RatingService
import de.gesundkrank.jskills.Rating
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

object PlayerStatisticsKeeper {
  sealed trait PlayerStatisticsDatabaseError extends DatabaseError
  final case object PlayerAlreadyExists extends PlayerStatisticsDatabaseError

  type PlayersStatisticsTableTuple = (Long, Long, Double, Double, Long, Long)

  object PlayerStatistics {
    def emptyPlayerStatistics: PlayerStatistics = {
      PlayerStatistics(0L, new Rating(RatingService.gameInfo.getInitialMean, RatingService.gameInfo.getInitialStandardDeviation), 0L, 0L)
    }

    def fromTuple(tuple: PlayersStatisticsTableTuple): PlayerStatistics = {
      PlayerStatistics(tuple._1, new Rating(tuple._3, tuple._4), tuple._5, tuple._6)
    }
  }

  case class PlayerStatistics(id: Long, rating: Rating, wins: Long, games: Long) {
    def *(playerDiscordId: UserId): PlayersStatisticsTableTuple = {
      (id, playerDiscordId.toUnsignedLong, rating.getMean, rating.getStandardDeviation, wins, games)
    }

    def formattedRating: String = f"${rating.getConservativeRating}%1.2f"

    def winRate: Double = if (games <= 0) 1.0 else wins.toDouble / games

    def winRatePercentage: String = f"${winRate * 100.0}%1.2f"
  }

  class PlayersStatistics(tag: Tag) extends Table[PlayersStatisticsTableTuple](tag, "playersStatistics") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def playerDiscordId: Rep[Long] = column[Long]("playerDiscordId")
    def ratingMean: Rep[Double] = column[Double]("ratingMean")
    def ratingStdDev: Rep[Double] = column[Double]("ratingStdDev")
    def wins: Rep[Long] = column[Long]("wins")
    def games: Rep[Long] = column[Long]("games")

    def * : ProvenShape[PlayersStatisticsTableTuple] =
      (id, playerDiscordId, ratingMean, ratingStdDev, wins, games)
  }

  val playersStatisticsTable: TableQuery[PlayersStatistics] = TableQuery[PlayersStatistics]
}

class PlayerStatisticsKeeper(databaseManager: DatabaseManager)(implicit ec: ExecutionContext) {
  import PlayerStatisticsKeeper._

  def insert(discordId: UserId): Future[Int] = {
    databaseManager.runTransaction {
      playersStatisticsTable += PlayerStatistics.emptyPlayerStatistics.*(discordId)
    }
  }

  def find(id: Long): Future[Option[PlayerStatistics]] = databaseManager.runTransaction {
    playersStatisticsTable
      .filter(p => p.id === id)
      .result
      .headOption
      .map(_.map(PlayerStatistics.fromTuple))
  }

  def update(playerStatistics: PlayerStatistics): Future[Int] = databaseManager.runTransaction {
    playersStatisticsTable
      .filter(p => p.id === playerStatistics.id)
      .map(p => (p.ratingMean, p.ratingStdDev, p.wins, p.games))
      .update((playerStatistics.rating.getMean, playerStatistics.rating.getStandardDeviation, playerStatistics.wins, playerStatistics.games))
  }
}
