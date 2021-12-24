package com.scustoms.database.keepers

import ackcord.data.UserId
import com.scustoms.database.DatabaseManager
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.database.keepers.PlayerStatisticsKeeper.PlayerStatistics
import com.scustoms.services.QueueService
import de.gesundkrank.jskills.Rating

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

object PlayerKeeper {
  sealed trait PlayerDatabaseError extends DatabaseError
  final case object PlayerAlreadyExists extends PlayerDatabaseError
  final case object PlayerNotFound extends PlayerDatabaseError

  type PlayerTableTuple = (Long, Long, String, String, Long, Long, Long, Long, Long)

  case class PlayerWithStatistics(id: Long, discordId: UserId, discordUsername: String, gameUsername: String, top: PlayerStatistics,
                    jungle: PlayerStatistics, mid: PlayerStatistics, bot: PlayerStatistics, support: PlayerStatistics) {
    def getTopStatistics: (QueueService.Role, PlayerStatistics) = {
      Seq(
        (top.rating.getConservativeRating,     (QueueService.Top, top)),
        (jungle.rating.getConservativeRating,  (QueueService.Jungle, jungle)),
        (mid.rating.getConservativeRating,     (QueueService.Mid, mid)),
        (bot.rating.getConservativeRating,     (QueueService.Bot, bot)),
        (support.rating.getConservativeRating, (QueueService.Support, support))
      ).maxBy(_._1)._2
    }

    def getRoleStatistics(role: QueueService.Role): PlayerStatistics = {
      role match {
        case QueueService.Top     => top
        case QueueService.Jungle  => jungle
        case QueueService.Mid     => mid
        case QueueService.Bot     => bot
        case QueueService.Support => support
      }
    }

    def niceString(role: QueueService.Role): String =
      f"rating: ${getRoleStatistics(role).rating.getConservativeRating}%1.2f"

    def totalGames: Long = top.games + jungle.games + mid.games + bot.games + support.games

    def totalWins: Long = top.wins + jungle.wins + mid.wins + bot.wins + support.wins

    def toPlayer: Player = Player(id, discordId, discordUsername, gameUsername, top.id, jungle.id, mid.id, bot.id, support.id)

    def updatedRating(role: QueueService.Role, newRating: Rating): PlayerWithStatistics = role match {
      case QueueService.Top     => this.copy(top = top.copy(rating = newRating))
      case QueueService.Jungle  => this.copy(jungle = jungle.copy(rating = newRating))
      case QueueService.Mid     => this.copy(mid = mid.copy(rating = newRating))
      case QueueService.Bot     => this.copy(bot = bot.copy(rating = newRating))
      case QueueService.Support => this.copy(support = support.copy(rating = newRating))
    }
  }

  object Player {
    def fromTuple(tuple: PlayerTableTuple): Player = {
      Player(tuple._1, UserId(tuple._2), tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
    }
  }

  case class Player(id: Long, discordId: UserId, discordUsername: String, gameUsername: String, top: Long,
                    jungle: Long, mid: Long, bot: Long, support: Long)

  case class PlayerCreate(discordId: UserId, discordUsername: String, gameUsername: String, topStatistics: Long,
                          jungleStatistics: Long, midStatistics: Long, botStatistics: Long, supportStatistics: Long) {
    def * : PlayerTableTuple = {
      (0L, discordId.toUnsignedLong, discordUsername, gameUsername, topStatistics, jungleStatistics, midStatistics, botStatistics, supportStatistics)
    }
  }

  class Players(tag: Tag) extends Table[PlayerTableTuple](tag, "players") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def discordId: Rep[Long] = column[Long]("discordId")
    def discordUsername: Rep[String] = column[String]("discordUsername")
    def gameUsername: Rep[String] = column[String]("gameUsername")
    def topStatistics: Rep[Long] = column[Long]("topStatistics")
    def jungleStatistics: Rep[Long] = column[Long]("jungleStatistics")
    def midStatistics: Rep[Long] = column[Long]("midStatistics")
    def botStatistics: Rep[Long] = column[Long]("botStatistics")
    def supportStatistics: Rep[Long] = column[Long]("supportStatistics")

    def * : ProvenShape[PlayerTableTuple] =
      (id, discordId, discordUsername, gameUsername, topStatistics, jungleStatistics, midStatistics, botStatistics, supportStatistics)
  }

  val playersTable: TableQuery[Players] = TableQuery[Players]
}

class PlayerKeeper(databaseManager: DatabaseManager)(implicit ec: ExecutionContext) {
  import PlayerKeeper._

  def insert(playerCreate: PlayerCreate): Future[Int] = databaseManager.run {
    playersTable += playerCreate.*
  }

  def getAll: Future[Seq[Player]] = databaseManager.run {
    playersTable
      .result
      .map(_.map(Player.fromTuple))
  }

  def find(discordId: UserId): Future[Option[Player]] = databaseManager.run {
    playersTable
      .filter(p => p.discordId === discordId.toUnsignedLong)
      .result
      .headOption
      .map(_.map(Player.fromTuple))
  }

  def findAll(playerIds: Seq[UserId]): Future[Either[DatabaseError, Seq[Player]]] = databaseManager.run {
    val players = playerIds.map(_.toUnsignedLong)
    playersTable
      .filter(p => p.discordId.inSet(players))
      .result
      .map {
        case foundPlayers if foundPlayers.length == players.length =>
          Right(foundPlayers.map(Player.fromTuple))
        case _ =>
          Left(PlayerNotFound)
      }
  }

  def exists(discordId: UserId): Future[Boolean] = databaseManager.run {
    playersTable
      .filter(p => p.discordId === discordId.toUnsignedLong)
      .exists
      .result
  }

  def updateUsernames(discordId: UserId, discordUsername: String, gameUsername: String): Future[Int] = databaseManager.runTransaction {
    playersTable
      .filter(p => p.discordId === discordId.toUnsignedLong)
      .map(p => (p.discordUsername, p.gameUsername))
      .update((discordUsername, gameUsername))
  }
}
