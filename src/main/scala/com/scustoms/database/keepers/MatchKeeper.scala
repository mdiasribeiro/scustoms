package com.scustoms.database.keepers

import com.scustoms.database.DatabaseManager
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.services.MatchmakingService.MatchPlayer
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

object MatchKeeper {
  sealed trait MatchDatabaseError extends DatabaseError
  final case object WrongTeamSize extends MatchDatabaseError

  type StoredMatchTableTuple = (Long, Long, Long, Long, Long, Long, Long, Long, Long, Long, Long, Boolean)

  case class StoredMatch(id: Long, teamA: Seq[MatchPlayer], teamB: Seq[MatchPlayer], team1Won: Boolean) {
    require(teamA.length == 5 && teamB.length == 5, "Match teams require exactly 5 players each")

    val Seq(topTeam1, jungleTeam1, midTeam1, botTeam1, supportTeam1) = teamA
    val Seq(topTeam2, jungleTeam2, midTeam2, botTeam2, supportTeam2) = teamB

    def * : StoredMatchTableTuple = {
      (id, topTeam1.discordId.toUnsignedLong, jungleTeam1.discordId.toUnsignedLong, midTeam1.discordId.toUnsignedLong,
        botTeam1.discordId.toUnsignedLong, supportTeam1.discordId.toUnsignedLong, topTeam2.discordId.toUnsignedLong,
        jungleTeam2.discordId.toUnsignedLong, midTeam2.discordId.toUnsignedLong, botTeam2.discordId.toUnsignedLong,
        supportTeam2.discordId.toUnsignedLong, team1Won
      )
    }
  }

  class StoredMatches(tag: Tag) extends Table[StoredMatchTableTuple](tag, "storedMatches") {
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

  val storedMatchesTable: TableQuery[StoredMatches] = TableQuery[StoredMatches]
}

class MatchKeeper(databaseManager: DatabaseManager)(implicit ec: ExecutionContext) {
  import MatchKeeper._

  def insert(m: StoredMatch): Future[Either[DatabaseError, Int]] = {
    databaseManager.runTransaction {
      storedMatchesTable += m.*
    }.map(Right(_))
  }
}
