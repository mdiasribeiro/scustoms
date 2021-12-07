package com.scustoms.database

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.scustoms.database.keepers.PlayerKeeper
import slick.jdbc
import scala.concurrent.{Await, ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.duration.DurationInt

object DatabaseService {
  sealed trait DatabaseCommand
  final case class PostMessage(message: String) extends DatabaseCommand

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  implicit val db: jdbc.SQLiteProfile.backend.Database = Database.forConfig("scustoms.sqlite")

  val playerKeeper: PlayerKeeper = new PlayerKeeper

  def testDatabase(): Unit = {

    val db = Database.forConfig("scustoms.sqlite")
    try {
      val setup = DBIO.seq(
        (PlayerKeeper.playersTable.schema).createIfNotExists
      )
      val setupFuture: Future[Unit] = db.run(setup)
      Await.result(setupFuture, 10.seconds)
      println("Done")
      val q1 = for(p <- PlayerKeeper.playersTable) yield p.username
      db.stream(q1.result).foreach(println)
      val insert = DBIO.seq(
        PlayerKeeper.playersTable += (0, 168517789483532288L, "MauZaum")
      )
      val insertFuture = db.run(insert)
      insertFuture.onComplete( _ => db.stream(q1.result).foreach(println))
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
