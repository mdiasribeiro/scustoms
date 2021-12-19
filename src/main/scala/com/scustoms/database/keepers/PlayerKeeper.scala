package com.scustoms.database.keepers

import ackcord.data.UserId
import com.scustoms.QueueService
import com.scustoms.database.DatabaseService
import com.scustoms.database.trueskill.RatingService
import de.gesundkrank.jskills.{IPlayer, Rating}

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

object PlayerKeeper {
  sealed trait DatabaseError
  final case object PlayerAlreadyExists extends DatabaseError

  object Player {
    def fromTuple(tuple: (Long, Long, String, String, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)): Player = {
      val ratings = PlayerRatings(new Rating(tuple._5, tuple._6), new Rating(tuple._7, tuple._8),
        new Rating(tuple._9, tuple._10), new Rating(tuple._11, tuple._12), new Rating(tuple._13, tuple._14)
      )
      Player(tuple._1, UserId(tuple._2), tuple._3, tuple._4, ratings)
    }
  }

  case class PlayerRatings(top: Rating, jungle: Rating, mid: Rating, bot: Rating, support: Rating) {
    def getRoleRating(role: QueueService.Role): Rating = {
      role match {
        case QueueService.Top => top
        case QueueService.Jungle => jungle
        case QueueService.Mid => mid
        case QueueService.Bot => bot
        case QueueService.Support => support
        case QueueService.Fill => throw new Exception("Fill rating does not exist")
      }
    }
  }
  case class Player(id: Long, discordId: UserId, discordUsername: String, gameUsername: String, rating: PlayerRatings) extends IPlayer {
    def * : (Long, Long, String, String, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double) = {
      (0L, discordId.toUnsignedLong, discordUsername, gameUsername,
        rating.top.getMean, rating.top.getStandardDeviation,
        rating.jungle.getMean, rating.jungle.getStandardDeviation,
        rating.mid.getMean, rating.mid.getStandardDeviation,
        rating.bot.getMean, rating.bot.getStandardDeviation,
        rating.support.getMean, rating.support.getStandardDeviation
      )
    }
  }
  case class PlayerCreate(discordId: UserId, discordUsername: String, gameUsername: String) {
    def toPlayer: Player = {
      val initialMean = RatingService.gameInfo.getInitialMean
      val initialStd = RatingService.gameInfo.getInitialStandardDeviation
      val starterRatings = PlayerRatings(new Rating(initialMean, initialStd), new Rating(initialMean, initialStd),
        new Rating(initialMean, initialStd), new Rating(initialMean, initialStd), new Rating(initialMean, initialStd)
      )
      Player(0L, discordId, discordUsername, gameUsername, starterRatings)
    }
  }

  class Players(tag: Tag) extends Table[(Long, Long, String, String, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)](tag, "players") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def discordId: Rep[Long] = column[Long]("discordId")
    def discordUsername: Rep[String] = column[String]("discordUsername")
    def gameUsername: Rep[String] = column[String]("gameUsername")
    def topRatingMean: Rep[Double] = column[Double]("topRatingMean")
    def topRatingStdDev: Rep[Double] = column[Double]("topRatingStdDev")
    def jungleRatingMean: Rep[Double] = column[Double]("jungleRatingMean")
    def jungleRatingStdDev: Rep[Double] = column[Double]("jungleRatingStdDev")
    def midRatingMean: Rep[Double] = column[Double]("midRatingMean")
    def midRatingStdDev: Rep[Double] = column[Double]("midRatingStdDev")
    def botRatingMean: Rep[Double] = column[Double]("botRatingMean")
    def botRatingStdDev: Rep[Double] = column[Double]("botRatingStdDev")
    def supportRatingMean: Rep[Double] = column[Double]("supportRatingMean")
    def supportRatingStdDev: Rep[Double] = column[Double]("supportRatingStdDev")
    def * : ProvenShape[(Long, Long, String, String, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)] =
      (id, discordId, discordUsername, gameUsername, topRatingMean, topRatingStdDev, jungleRatingMean, jungleRatingStdDev,
        midRatingMean, midRatingStdDev, botRatingMean, botRatingStdDev, supportRatingMean, supportRatingStdDev)
  }

  val playersTable: TableQuery[Players] = TableQuery[Players]
}

class PlayerKeeper(implicit ec: ExecutionContext) {
  import PlayerKeeper._

  def insert(playerCreate: PlayerCreate): Future[Either[DatabaseError, Int]] = {
    this.exists(playerCreate.discordId).flatMap {
      case true =>
        Future.successful(Left(PlayerAlreadyExists))
      case false =>
        DatabaseService.runTransaction {
          playersTable += playerCreate.toPlayer.*
        }.map(Right(_))
    }
  }

  def find(discordId: UserId): Future[Option[Player]] = DatabaseService.runTransaction {
    playersTable
      .filter(p => p.discordId === discordId.toUnsignedLong)
      .result
      .headOption
      .map(_.map(Player.fromTuple))
  }

  def findAll(playerIds: Seq[UserId]): Future[Seq[Player]] = DatabaseService.runTransaction {
    val players = playerIds.map(_.toUnsignedLong)
    playersTable
      .filter(p => p.discordId.inSet(players))
      .result
      .map(_.map(Player.fromTuple))
  }

  def exists(discordId: UserId): Future[Boolean] = DatabaseService.runTransaction {
    playersTable
      .filter(p => p.discordId === discordId.toUnsignedLong)
      .exists
      .result
  }

  def updateUsernames(player: Player): Future[Int] = DatabaseService.runTransaction {
    playersTable
      .filter(p => p.discordId === player.discordId.toUnsignedLong)
      .map(p => (p.gameUsername, p.discordUsername, p.gameUsername))
      .update((player.gameUsername, player.discordUsername, player.gameUsername))
  }

  def updateRating(player: Player, role: QueueService.Role): Future[Int] = DatabaseService.runTransaction {
    val filteredTable = playersTable.filter(p => p.discordId === player.discordId.toUnsignedLong)
    role match {
      case QueueService.Top =>
        filteredTable
          .map(p => (p.gameUsername, p.topRatingMean, p.topRatingStdDev))
          .update((player.gameUsername, player.rating.top.getMean, player.rating.top.getStandardDeviation))
      case QueueService.Jungle =>
        filteredTable
          .map(p => (p.gameUsername, p.jungleRatingMean, p.jungleRatingStdDev))
          .update((player.gameUsername, player.rating.jungle.getMean, player.rating.jungle.getStandardDeviation))
      case QueueService.Mid =>
        filteredTable
          .map(p => (p.gameUsername, p.midRatingMean, p.midRatingStdDev))
          .update((player.gameUsername, player.rating.mid.getMean, player.rating.mid.getStandardDeviation))
      case QueueService.Bot =>
        filteredTable
          .map(p => (p.gameUsername, p.botRatingMean, p.botRatingStdDev))
          .update((player.gameUsername, player.rating.bot.getMean, player.rating.bot.getStandardDeviation))
      case QueueService.Support =>
        filteredTable
          .map(p => (p.gameUsername, p.supportRatingMean, p.supportRatingStdDev))
          .update((player.gameUsername, player.rating.support.getMean, player.rating.support.getStandardDeviation))
      case QueueService.Fill =>
        throw new Exception("Fill rating does not exist")
    }
  }
}
