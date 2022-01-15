package com.scustoms.database.keepers

import ackcord.data.UserId
import com.scustoms.database.DatabaseManager
import com.scustoms.database.DatabaseManager._
import com.scustoms.trueskill.RatingUtils
import com.scustoms.trueskill.RatingUtils.{percentageFormat, ratingFormat}
import de.gesundkrank.jskills.{GameInfo, Rating}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

object PlayerStatisticsKeeper {
  type PlayersStatisticsTableTuple = (Long, Long, Double, Double, Long, Long)

  object PlayerStatistics {
    def emptyPlayerStatistics: PlayerStatistics = {
      PlayerStatistics(0L, new Rating(RatingUtils.defaultGameInfo.getInitialMean, RatingUtils.defaultGameInfo.getInitialStandardDeviation), 0L, 0L)
    }

    def fromTuple(tuple: PlayersStatisticsTableTuple): PlayerStatistics = {
      PlayerStatistics(tuple._1, new Rating(tuple._3, tuple._4), tuple._5, tuple._6)
    }
  }

  case class PlayerStatistics(id: Long, rating: Rating, wins: Long, games: Long) {
    def *(playerDiscordId: UserId): PlayersStatisticsTableTuple = {
      (id, playerDiscordId.toUnsignedLong, rating.getMean, rating.getStandardDeviation, wins, games)
    }

    def formattedMeanRating: String = ratingFormat(rating.getMean)

    def formattedConservativeRating: String = ratingFormat(rating.getConservativeRating)

    def winRate: Double = if (games <= 0) 1.0 else wins.toDouble / games.toDouble

    def winRatePercentage: String = percentageFormat(winRate * 100.0)

    def updated(newRating: Rating, won: Boolean): PlayerStatistics =
      this.copy(rating = newRating, games = games + 1, wins = wins + (if (won) 1 else 0))
  }

  class PlayerStatisticsTableSchema(tag: Tag) extends Table[PlayersStatisticsTableTuple](tag, "playersStatistics") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def playerDiscordId: Rep[Long] = column[Long]("playerDiscordId", O.Unique)
    def ratingMean: Rep[Double] = column[Double]("ratingMean")
    def ratingStdDev: Rep[Double] = column[Double]("ratingStdDev")
    def wins: Rep[Long] = column[Long]("wins")
    def games: Rep[Long] = column[Long]("games")

    def * : ProvenShape[PlayersStatisticsTableTuple] =
      (id, playerDiscordId, ratingMean, ratingStdDev, wins, games)
  }

  def defaultErrorHandler[T]: PartialFunction[Throwable, Either[DatabaseError, T]] = {
    case err => Left(UnexpectedError(err.getMessage))
  }

  val playersStatisticsTable: TableQuery[PlayerStatisticsTableSchema] = TableQuery[PlayerStatisticsTableSchema]
}

class PlayerStatisticsKeeper(databaseManager: DatabaseManager)(implicit ec: ExecutionContext) {
  import PlayerStatisticsKeeper._

  def insert(discordId: UserId): Future[Either[DatabaseError, Long]] =
    databaseManager.run {
      (playersStatisticsTable returning playersStatisticsTable.map(_.id) += PlayerStatistics.emptyPlayerStatistics.*(discordId))
        .map {
          case 0 => Left(FailedInsertion: DatabaseError)
          case n => Right(n)
        }
    }(PlayerStatisticsKeeper.defaultErrorHandler)

  def insertAllRoles(discordId: UserId): Future[Either[DatabaseError, Seq[Long]]] =
    databaseManager.runTransaction {
      (playersStatisticsTable returning playersStatisticsTable.map(_.id) ++= Seq(
        PlayerStatistics.emptyPlayerStatistics.*(discordId),
        PlayerStatistics.emptyPlayerStatistics.*(discordId),
        PlayerStatistics.emptyPlayerStatistics.*(discordId),
        PlayerStatistics.emptyPlayerStatistics.*(discordId),
        PlayerStatistics.emptyPlayerStatistics.*(discordId)
      )).map {
        case s if s.isEmpty | s.contains(0L) => Left(FailedInsertion: DatabaseError)
        case s => Right(s)
      }

    }(PlayerStatisticsKeeper.defaultErrorHandler)

  def find(id: Long): Future[Either[DatabaseError, PlayerStatistics]] =
    databaseManager.run {
      playersStatisticsTable
        .filter(p => p.id === id)
        .result
        .headOption
        .map {
          case Some(statisticsTuple) => Right(PlayerStatistics.fromTuple(statisticsTuple))
          case None => Left(StatisticsNotFound: DatabaseError)
        }
    }(PlayerStatisticsKeeper.defaultErrorHandler)

  def update(playerStatistics: PlayerStatistics): Future[Boolean] =
    databaseManager.runTransaction {
      playersStatisticsTable
        .filter(p => p.id === playerStatistics.id)
        .map(p => (p.ratingMean, p.ratingStdDev, p.wins, p.games))
        .update((playerStatistics.rating.getMean, playerStatistics.rating.getStandardDeviation, playerStatistics.wins, playerStatistics.games))
        .map(_ > 0)
    } {
      case _ => false
    }

  def resetAll(gameInfo: GameInfo): Future[Int] =
    databaseManager.runTransaction {
      playersStatisticsTable
        .map(p => (p.ratingMean, p.ratingStdDev, p.wins, p.games))
        .update((gameInfo.getInitialMean, gameInfo.getInitialStandardDeviation, 0, 0))
    } {
      case _ => 0
    }
}
