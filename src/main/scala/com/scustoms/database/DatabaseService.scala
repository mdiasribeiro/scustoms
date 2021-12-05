package com.scustoms.database

import ackcord.APIMessage
import ackcord.data.TextGuildChannel
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import java.io.File
import scala.concurrent.{Await, ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.duration.DurationInt

object DatabaseService {
  sealed trait DatabaseCommand
  final case class PostMessage(message: String) extends DatabaseCommand

  class Players(tag: Tag) extends Table[(Long, Long, String)](tag, "players") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def discordId = column[Long]("discordId")
    def username = column[String]("username")
    def * = (id, discordId, username)
  }

  def testDatabase(): Unit = {
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

    val playersTable = TableQuery[Players]
    val db = Database.forConfig("scustoms.sqlite")
    try {
      val setup = DBIO.seq(
        (playersTable.schema).createIfNotExists
      )
      val setupFuture: Future[Unit] = db.run(setup)
      Await.result(setupFuture, 10.seconds)
      println("Done")
      val q1 = for(p <- playersTable) yield p.username
      db.stream(q1.result).foreach(println)
      val insert = DBIO.seq(
        playersTable += (0, 168517789483532288L, "MauZaum")
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
