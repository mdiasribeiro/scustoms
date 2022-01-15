package com.scustoms.database.keepers

import ackcord.data.UserId
import com.scustoms.database.DatabaseManager
import com.scustoms.database.DatabaseManager._

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

import java.sql.SQLException

object PlayerKeeper {
  type PlayerTableTuple = (Long, Long, String, String, Long, Long, Long, Long, Long)

  object StoredPlayer {
    def fromTuple(tuple: PlayerTableTuple): StoredPlayer = {
      StoredPlayer(tuple._1, UserId(tuple._2), tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9)
    }
  }

  case class StoredPlayer(id: Long, discordId: UserId, discordUsername: String, gameUsername: String, topStatsId: Long,
                          jungleStatsId: Long, midStatsId: Long, botStatsId: Long, supportStatsId: Long) {
    def * : PlayerTableTuple = {
      (id, discordId.toUnsignedLong, discordUsername, gameUsername, topStatsId, jungleStatsId, midStatsId, botStatsId, supportStatsId)
    }
  }

  class PlayerTableSchema(tag: Tag) extends Table[PlayerTableTuple](tag, "players") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def discordId: Rep[Long] = column[Long]("discordId", O.Unique)
    def discordUsername: Rep[String] = column[String]("discordUsername")
    def gameUsername: Rep[String] = column[String]("gameUsername")
    def topStatsId: Rep[Long] = column[Long]("topStatsId", O.Unique)
    def jungleStatsId: Rep[Long] = column[Long]("jungleStatsId", O.Unique)
    def midStatsId: Rep[Long] = column[Long]("midStatsId", O.Unique)
    def botStatsId: Rep[Long] = column[Long]("botStatsId", O.Unique)
    def supportStatsId: Rep[Long] = column[Long]("supportStatsId", O.Unique)

    def * : ProvenShape[PlayerTableTuple] =
      (id, discordId, discordUsername, gameUsername, topStatsId, jungleStatsId, midStatsId, botStatsId, supportStatsId)
  }

  def defaultErrorHandler[T]: PartialFunction[Throwable, Either[DatabaseError, T]] = {
    case err => Left(UnexpectedError(err.getMessage))
  }

  val playersTable: TableQuery[PlayerTableSchema] = TableQuery[PlayerTableSchema]
}

class PlayerKeeper(databaseManager: DatabaseManager)(implicit ec: ExecutionContext) {
  import PlayerKeeper._

  def insert(playerCreate: StoredPlayer): Future[Either[DatabaseError, Long]] = {
    databaseManager.runTransaction {
      (playersTable returning playersTable.map(_.id) += playerCreate.*)
        .map {
          case 0 => Left(PlayerAlreadyExists: DatabaseError)
          case id => Right(id)
        }
    } {
      case err: SQLException =>
        println(s"Got SQL exception: ${err.getMessage}")
        Left(PlayerAlreadyExists)
      case err: Throwable =>
        Left(UnexpectedError(err.getMessage))
    }
  }

  def getAll: Future[Seq[StoredPlayer]] = databaseManager.run {
    playersTable
      .result
      .map(_.map(StoredPlayer.fromTuple))
  }()

  def find(discordId: UserId): Future[Either[DatabaseError, StoredPlayer]] =
    databaseManager.run {
      playersTable
        .filter(p => p.discordId === discordId.toUnsignedLong)
        .result
        .headOption
        .map {
          case Some(playerTuple) => Right(StoredPlayer.fromTuple(playerTuple))
          case None => Left(PlayerNotFound: DatabaseError)
        }
    }(PlayerKeeper.defaultErrorHandler)

  def findAll(playerIds: Seq[UserId]): Future[Either[DatabaseError, Seq[StoredPlayer]]] =
    databaseManager.run {
      val players = playerIds.map(_.toUnsignedLong)
      playersTable
        .filter(p => p.discordId.inSet(players))
        .result
        .map {
          case foundPlayers if foundPlayers.length == players.length =>
            Right(foundPlayers.map(StoredPlayer.fromTuple))
          case _ =>
            Left(PlayerNotFound: DatabaseError)
        }
    }(PlayerKeeper.defaultErrorHandler)

  def exists(discordId: UserId): Future[Boolean] =
    databaseManager.run {
      playersTable
        .filter(p => p.discordId === discordId.toUnsignedLong)
        .exists
        .result
    }()

  def updateGameUsername(discordId: UserId, gameUsername: String): Future[Boolean] =
    databaseManager.runTransaction {
      playersTable
        .filter(p => p.discordId === discordId.toUnsignedLong)
        .map(p => p.gameUsername)
        .update(gameUsername)
        .map(_ > 0)
    }()
}
