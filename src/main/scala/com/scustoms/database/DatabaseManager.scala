package com.scustoms.database

import com.scustoms.database.keepers.{MatchKeeper, PlayerKeeper, PlayerStatisticsKeeper}
import slick.{dbio, jdbc}

import scala.concurrent.{Await, ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.duration.DurationInt

object DatabaseManager {
  trait DatabaseError
}

class DatabaseManager {
  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  implicit val db: jdbc.SQLiteProfile.backend.Database = Database.forConfig("scustoms.sqlite")

  def clearDatabase(): Future[Unit] = {
    runTransaction {
      DBIO.seq(
        PlayerKeeper.playersTable.schema.dropIfExists,
        PlayerStatisticsKeeper.playersStatisticsTable.schema.dropIfExists,
        MatchKeeper.storedMatchesTable.schema.dropIfExists
      )
    }
  }

  def setupDatabase(): Future[Unit] = {
    runTransaction {
      DBIO.seq(
        PlayerKeeper.playersTable.schema.createIfNotExists,
        PlayerStatisticsKeeper.playersStatisticsTable.schema.createIfNotExists,
        MatchKeeper.storedMatchesTable.schema.createIfNotExists
      )
    }
  }

  // https://scala-slick.org/doc/3.2.0/dbio.html#executing-database-i-o-actions
  def runTransaction[R, S <: dbio.NoStream, E <: dbio.Effect](operations: => DBIOAction[R, S, E]): Future[R] = {
    db.run(operations.transactionally)
  }

  def run[R, S <: dbio.NoStream, E <: dbio.Effect](operations: => DBIOAction[R, S, E]): Future[R] = {
    db.run(operations)
  }

  def runSync[R, S <: dbio.NoStream, E <: dbio.Effect](operations: => DBIOAction[R, S, E]): R = {
    try {
      Await.result(db.run(operations), 10.seconds)
    } catch {
      case err: Exception =>
        println(s"Error in future: $err")
        throw err
    }
  }
}
