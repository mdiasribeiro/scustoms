package com.scustoms.database

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.scustoms.database.keepers.PlayerKeeper
import slick.{dbio, jdbc}

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._

object DatabaseService {
  sealed trait DatabaseCommand
  final case class PostMessage(message: String) extends DatabaseCommand

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  implicit val db: jdbc.SQLiteProfile.backend.Database = Database.forConfig("scustoms.sqlite")

  val playerKeeper: PlayerKeeper = new PlayerKeeper

  def clearDatabase(): Future[Unit] = {
    runTransaction {
      DBIO.seq(
        PlayerKeeper.playersTable.schema.dropIfExists
      )
    }
  }

  def setupDatabase(): Future[Unit] = {
    runTransaction {
      DBIO.seq(
        PlayerKeeper.playersTable.schema.createIfNotExists
      )
    }
  }

  // https://scala-slick.org/doc/3.2.0/dbio.html#executing-database-i-o-actions
  def runTransaction[R, S <: dbio.NoStream, E <: dbio.Effect](operations: => DBIOAction[R, S, E]): Future[R] = {
    try {
      val db = Database.forConfig("scustoms.sqlite")
      val ops = operations
      db.run(ops.transactionally)
    } finally db.close
  }

  def apply(): Behavior[DatabaseCommand] =
    Behaviors.setup { context =>

      Behaviors.receiveMessage {
        case PostMessage(message) =>
          println(s"Posted message: $message")
          Behaviors.same
      }
    }
}
