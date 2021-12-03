package com.scustoms

import ackcord.{CacheSnapshot, DiscordClient}
import ackcord.commands.{CommandController, HelpCommand, NamedCommand, UserCommandMessage}
import ackcord.data.{Emoji, Message, SnowflakeType, UserId}
import ackcord.requests.{CreateMessage, CreateMessageData, CreateReaction}
import akka.NotUsed
import ackcord.syntax.TextChannelSyntax

import scala.concurrent.Future
import scala.util.Random

class BotCommands(client: DiscordClient, shutdownHook: () => Unit) extends CommandController(client.requests) {

  final case class Player(id: Long, name: String, lines: Vector[String])

  def isFromAdmin(id: UserId): Boolean = {
    id.toUnsignedLong == 138822865708515329L
  }

  def adminGate(command: UserCommandMessage[NotUsed])(f: => CreateMessage) = {
    command.user.discriminator
    if (isFromAdmin(command.user.id)) {
      f
    } else {
      implicit val c: CacheSnapshot = command.cache
      client.requestsHelper.runMany(
        CreateReaction(command.textChannel.id, command.message.id, ":kekw:"),
        command.textChannel.sendMessage(s"Vai-te foder ${command.user.username}, não mandas em mim!")
      )
    }
  }

  val userMap = Vector(
    Player(349606871210000385L, "Giwinho", Vector("Elo: 1200 (se for the Ashe meio, é 300)")),
    Player(297130307314778112L, "States", Vector("O pior jungler da actualidade")),
    Player(183728707368648704L, "Batatas", Vector.empty),
    Player(169131637269987328L, "Ramos", Vector("Larga o LoL e vai mudar a fralda da Madalena")),
    Player(298087965392109570L, "Krop", Vector.empty),
    Player(298087495650902016L, "Shikishoku", Vector.empty),
    Player(515150225434738700L, "Paulo", Vector.empty),
    Player(168528530559336448L, "Sakini", Vector.empty),
    Player(296761425982914581L, "Careca", Vector.empty),
    Player(138822865708515329L, "H4uZ", Vector.empty),
    Player(164791190208774144L, "kn0x", Vector.empty),
    Player(168517789483532288L, "MauZaum", Vector.empty)
  )
  val commandSymbols = Seq("$")
  val kekw: String = SnowflakeType[Emoji]("<:kekw:811007367101415495>").toString
  val randomGenerator = new Random()

  val hello: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("hello"))
    .withRequest(m => m.textChannel.sendMessage(s"Hello ${m.user.username}!"))

  val status: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("status"))
    .withRequest(m => {
      m.textChannel.sendMessage(s"I'm currently running version '0.03' on 'docker-compose-host'.")
    })

  val info: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("info"))
    .withRequest(m => {
      val messageToSend = userMap.find(u => u.id.toString == m.user.id.toString) match {
        case Some(Player(id, name, lines)) if lines.isEmpty =>
          s"Não sei nada sobre ti, $name"
        case Some(Player(id, name, lines)) =>
          s"${lines(randomGenerator.nextInt(lines.size))}"
        case None =>
          s"Não te conheço de lado nenhum"
      }
      m.textChannel.sendMessage(messageToSend)
    })

  val react: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("react"))
    .withRequest(m => CreateReaction(m.textChannel.id, m.message.id, kekw))

  val shutdown: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("shutdown"))
    .withSideEffects(m => {
      adminGate(m) {
        shutdownHook()
        m.textChannel.sendMessage("Shutting down :(")
      }
    })
}
