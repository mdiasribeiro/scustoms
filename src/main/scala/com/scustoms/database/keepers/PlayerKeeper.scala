package com.scustoms.database.keepers

import ackcord.data.UserId

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

object PlayerKeeper {
  sealed trait DatabaseError
  final case object PlayerAlreadyExists extends DatabaseError

  case class Player(id: Long, discordId: Long, username: String)
  case class PlayerCreate(discordId: Long, username: String) {
    def * : (Long, Long, String) = (0L, discordId, username)
  }

  class Players(tag: Tag) extends Table[(Long, Long, String)](tag, "players") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def discordId: Rep[Long] = column[Long]("discordId")
    def username: Rep[String] = column[String]("username")
    def * : ProvenShape[(Long, Long, String)] = (id, discordId, username)
  }

  val playersTable: TableQuery[Players] = TableQuery[Players]
}

class PlayerKeeper(implicit db: Database, ec: ExecutionContext) {
  import PlayerKeeper._

  def insertNewPlayer(player: PlayerCreate): Future[Either[DatabaseError, Int]] = {
    playerExists(player.discordId).flatMap {
      case true =>
        Future.successful(Left(PlayerAlreadyExists))
      case false =>
        db.run(playersTable += player.*).map(Right(_))
    }
  }

  def findPlayer(discordId: Long): Future[Option[Player]] = {
    val find = playersTable
      .filter(p => p.discordId === discordId)
      .result
      .headOption
      .map(_.map(p => Player(p._1, p._2, p._3)))
    db.run(find)
  }

  def playerExists(discordId: Long): Future[Boolean] = {
    val find = playersTable.filter(p => p.discordId === discordId).exists.result
    db.run(find)
  }

  def playerExists(discordId: UserId): Future[Boolean] = playerExists(discordId.toUnsignedLong)
}
