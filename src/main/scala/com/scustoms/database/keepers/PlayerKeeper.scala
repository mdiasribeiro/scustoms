package com.scustoms.database.keepers

import ackcord.data.UserId
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
    def fromTuple(tuple: (Long, Long, String, String, Double, Double)): Player = {
      Player(tuple._1, UserId(tuple._2), tuple._3, tuple._4, new Rating(tuple._5, tuple._6))
    }
  }
  case class Player(id: Long, discordId: UserId, discordUsername: String, gameUsername: String, rating: Rating) extends IPlayer
  case class PlayerCreate(discordId: UserId, discordUsername: String, gameUsername: String) {
    def * : (Long, Long, String, String, Double, Double) = (0L, discordId.toUnsignedLong, discordUsername, gameUsername, RatingService.gameInfo.getInitialMean, RatingService.gameInfo.getInitialStandardDeviation)
  }

  class Players(tag: Tag) extends Table[(Long, Long, String, String, Double, Double)](tag, "players") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def discordId: Rep[Long] = column[Long]("discordId")
    def discordUsername: Rep[String] = column[String]("discordUsername")
    def gameUsername: Rep[String] = column[String]("gameUsername")
    def ratingMean: Rep[Double] = column[Double]("ratingMean")
    def ratingStdDev: Rep[Double] = column[Double]("ratingStdDev")
    def * : ProvenShape[(Long, Long, String, String, Double, Double)] = (id, discordId, discordUsername, gameUsername, ratingMean, ratingStdDev)
  }

  val playersTable: TableQuery[Players] = TableQuery[Players]
}

class PlayerKeeper(implicit ec: ExecutionContext) {
  import PlayerKeeper._

  def insert(player: PlayerCreate): Future[Either[DatabaseError, Int]] = {
    this.exists(player.discordId).flatMap {
      case true =>
        Future.successful(Left(PlayerAlreadyExists))
      case false =>
        DatabaseService.runTransaction {
          playersTable += player.*
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

  def update(player: Player): Future[Int] = DatabaseService.runTransaction {
    playersTable
      .filter(p => p.discordId === player.discordId.toUnsignedLong)
      .map(p => (p.gameUsername, p.ratingMean, p.ratingStdDev))
      .update((player.gameUsername, player.rating.getMean, player.rating.getStandardDeviation))
  }
}
