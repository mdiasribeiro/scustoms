package com.scustoms

import ackcord.commands.{CommandController, NamedCommand}
import ackcord.data.{TextGuildChannel, UserId}
import ackcord.requests.Requests
import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings}
import akka.actor.typed.Behavior
import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

object TestBot {
  sealed trait SessionCommand
  final case class PostMessage(message: String) extends SessionCommand

  final case class User(id: Long, name: String, frases: Vector[String])

  val token = "OTE2MDQ0OTY2NDI4NTUzMjc4.YakbWw.mG3-BRCAyxPC4-e7FHJrr7wOsV8"
  val clientSettings: ClientSettings = ClientSettings(token)
  val botChannel = "scustoms"
  val random = new Random

  val userMap = Vector(
    User(349606871210000385L, "Giwinho", Vector("Elo: 1200 (se for the Ashe meio, é 300)")),
    User(297130307314778112L, "States", Vector.empty),
    User(183728707368648704L, "Batatas", Vector.empty),
    User(169131637269987328L, "Ramos", Vector("Larga o LoL e vai mudar a fralda da Madalena")),
    User(298087965392109570L, "Krop", Vector.empty),
    User(298087495650902016L, "Shikishoku", Vector.empty),
    User(515150225434738700L, "Paulo", Vector.empty),
    User(168528530559336448L, "Sakini", Vector.empty),
    User(296761425982914581L, "Careca", Vector.empty),
    User(138822865708515329L, "H4uZ", Vector.empty),
    User(164791190208774144L, "kn0x", Vector.empty),
    User(168517789483532288L, "MauZaum", Vector.empty)
  )

  class MyCommands(requests: Requests) extends CommandController(requests) {
    val hello: NamedCommand[NotUsed] = Command
      .named(Seq("$"), Seq("hello"))
      .withRequest(m => {
        val withEmail = m.user.email.map(email => s", I'll spam you at $email").getOrElse("")
        m.textChannel.sendMessage(s"Hello ${m.user.username}$withEmail!")
      })

    val info: NamedCommand[NotUsed] = Command
      .named(Seq("$"), Seq("info"))
      .withRequest(m => {
        val messageToSend = userMap.find(u => u.id.toString == m.user.id.toString) match {
          case Some(User(id, name, lines)) if lines.isEmpty =>
            s"Não sei nada sobre ti, $name"
          case Some(User(id, name, lines)) =>
            s"${lines(random.nextInt(lines.size))}"
          case None =>
            s"Não te conheço de lado nenhum"
        }
        println(messageToSend)
        m.textChannel.sendMessage(messageToSend)
      })
  }

  def apply(): Behavior[SessionCommand] =
    Behaviors.setup { context =>
      val client = Await.result(clientSettings.createClient(), 10.seconds)

      val myCommands = new MyCommands(client.requests)
      client.commands.bulkRunNamed(myCommands.hello, myCommands.info)

      client.onEventSideEffectsIgnore {
        case APIMessage.Ready(_) => println("Now ready")
      }

      client.onEventSideEffects { c => {
        case APIMessage.MessageCreate(_, message, _) => println(c.getTextChannel(message.channelId))
      }}

      client.onEventAsync { implicit c => {
        case APIMessage.ChannelCreate(_, channel: TextGuildChannel, _) =>
          client.requestsHelper.run(channel.sendMessage(content = "Não se aprende nada aqui")).map(_ => ())(client.executionContext)
      }}

      client.login()

      Behaviors.receiveMessage {
        case PostMessage(message) =>
          println(s"Posted message: $message")
          Behaviors.same
      }
    }
}
