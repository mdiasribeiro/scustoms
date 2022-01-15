package com.scustoms.database.keepers

import ackcord.data.UserId
import com.scustoms.database.DatabaseManager
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

object MatchKeeper {
  type StoredMatchTableTuple = (Long, Long, Long, Long, Long, Long, Long, Long, Long, Long, Long, Boolean)

  case class StoredMatchTeam(topId: UserId, jungleId: UserId, midId: UserId, botId: UserId, supportId: UserId)

  object StoredMatch {
    def fromTuple(t: StoredMatchTableTuple): StoredMatch =
      StoredMatch(t._1,
        StoredMatchTeam(UserId(t._2), UserId(t._3), UserId(t._4), UserId(t._5), UserId(t._6)),
        StoredMatchTeam(UserId(t._7), UserId(t._8), UserId(t._9), UserId(t._10), UserId(t._11)),
        t._12
      )
  }

  case class StoredMatch(id: Long, teamA: StoredMatchTeam, teamB: StoredMatchTeam, team1Won: Boolean) {
    def * : StoredMatchTableTuple = {
      (id, teamA.topId.toUnsignedLong, teamA.jungleId.toUnsignedLong, teamA.midId.toUnsignedLong,
        teamA.botId.toUnsignedLong, teamA.supportId.toUnsignedLong, teamB.topId.toUnsignedLong,
        teamB.jungleId.toUnsignedLong, teamB.midId.toUnsignedLong, teamB.botId.toUnsignedLong,
        teamB.supportId.toUnsignedLong, team1Won
      )
    }
  }

  class MatchTableSchema(tag: Tag) extends Table[StoredMatchTableTuple](tag, "storedMatches") {
    def id: Rep[Long] = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def topTeam1: Rep[Long] = column[Long]("topTeam1")
    def jungleTeam1: Rep[Long] = column[Long]("jungleTeam1")
    def midTeam1: Rep[Long] = column[Long]("midTeam1")
    def botTeam1: Rep[Long] = column[Long]("botTeam1")
    def supportTeam1: Rep[Long] = column[Long]("supportTeam1")
    def topTeam2: Rep[Long] = column[Long]("topTeam2")
    def jungleTeam2: Rep[Long] = column[Long]("jungleTeam2")
    def midTeam2: Rep[Long] = column[Long]("midTeam2")
    def botTeam2: Rep[Long] = column[Long]("botTeam2")
    def supportTeam2: Rep[Long] = column[Long]("supportTeam2")
    def team1Won: Rep[Boolean] = column[Boolean]("team1Won")
    def * : ProvenShape[StoredMatchTableTuple] =
      (id, topTeam1, jungleTeam1, midTeam1, botTeam1, supportTeam1, topTeam2, jungleTeam2,
        midTeam2, botTeam2, supportTeam2, team1Won)
  }

  val storedMatchesTable: TableQuery[MatchTableSchema] = TableQuery[MatchTableSchema]
}

class MatchKeeper(databaseManager: DatabaseManager)(implicit ec: ExecutionContext) {
  import MatchKeeper._

  def insert(m: StoredMatch): Future[Int] =
    databaseManager.run {
      storedMatchesTable += m.*
    }()

  def getLastN(n: Int): Future[Seq[StoredMatch]] =
    databaseManager.run {
      storedMatchesTable
        .sortBy(_.id.desc)
        .take(n)
        .result
        .map(_.map(StoredMatch.fromTuple))
    }()

  def get(limit: Int, offset: Int): Future[Seq[StoredMatch]] =
    databaseManager.run {
      storedMatchesTable
        .sortBy(_.id)
        .drop(offset)
        .take(limit)
        .result
        .map(_.map(StoredMatch.fromTuple))
    }()

  def changeResult(id: Long, team1Won: Boolean): Future[Int] =
    databaseManager.run {
      storedMatchesTable
        .filter(storedMatch => storedMatch.id === id)
        .map(p => p.team1Won)
        .update(team1Won)
    }()

  def remove(id: Long): Future[Int] =
    databaseManager.run {
      storedMatchesTable
        .filter(storedMatch => storedMatch.id === id)
        .delete
    }()
}
