package com.scustoms.database

import com.scustoms.database.keepers.{MatchKeeper, PlayerKeeper, PlayerStatisticsKeeper}
import slick.{dbio, jdbc}

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._

object DatabaseManager {
  trait DatabaseError {
    def message: String
  }
}

class DatabaseManager {
  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  implicit val db: jdbc.SQLiteProfile.backend.Database = Database.forConfig("scustoms.sqlite")

  def logAndRethrow[R]: PartialFunction[Throwable, R] = {
    case err =>
      println(s"An error occurred: ${err.getMessage}")
      throw err
  }

  def clearDatabase(): Future[Unit] = {
    runTransaction {
      DBIO.seq(
        PlayerKeeper.playersTable.schema.dropIfExists,
        PlayerStatisticsKeeper.playersStatisticsTable.schema.dropIfExists
      )
    }()
  }

  def setupDatabase(): Future[Unit] = {
    runTransaction {
      DBIO.seq(
        PlayerKeeper.playersTable.schema.createIfNotExists,
        PlayerStatisticsKeeper.playersStatisticsTable.schema.createIfNotExists,
        MatchKeeper.storedMatchesTable.schema.createIfNotExists
      )
    }()
  }

  // https://scala-slick.org/doc/3.2.0/dbio.html#executing-database-i-o-actions
  def runTransaction[R, S <: dbio.NoStream, E <: dbio.Effect]
  (operations: => DBIOAction[R, S, E])
  (errorHandler: PartialFunction[Throwable, R] = logAndRethrow): Future[R] = {
    db.run(operations.transactionally).recover(errorHandler)
  }

  def run[R, S <: dbio.NoStream, E <: dbio.Effect]
  (operations: => DBIOAction[R, S, E])
  (errorHandler: PartialFunction[Throwable, R] = logAndRethrow): Future[R] = {
    db.run(operations).recover(errorHandler)
  }
}
