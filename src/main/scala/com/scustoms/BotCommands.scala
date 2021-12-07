package com.scustoms

import ackcord.{DiscordClient, OptFuture}
import ackcord.commands.{CommandController, NamedCommand}
import ackcord.data.UserId
import ackcord.requests.CreateReaction
import akka.NotUsed
import ackcord.syntax.TextChannelSyntax
import com.scustoms.Emojis.{checkMark, negativeMark}
import com.scustoms.database.DatabaseService
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.services.AdminService
import com.typesafe.config.{Config, ConfigFactory}
import slick.jdbc.JdbcBackend
import slick.jdbc.JdbcBackend.Database

import scala.concurrent.Future
import scala.util.Random

class BotCommands(client: DiscordClient, shutdownHook: () => Unit) extends CommandController(client.requests) {

  val config: Config = ConfigFactory.load()
  val commandSymbols = Seq(config.getString("scustoms.commandSymbol"))

  implicit val db: JdbcBackend.Database = Database.forConfig("scustoms.sqlite")

  val randomGenerator = new Random()

  val hello: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("hello"))
    .withRequest(m => m.textChannel.sendMessage(s"Hello ${m.user.username}!"))

  val status: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("status"))
    .withRequest(m => {
      m.textChannel.sendMessage(s"I'm currently running version '0.03' on 'docker-compose-host'")
    })

  val info: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("info"))
    .withRequest(m => {
      m.textChannel.sendMessage(s"TBA")
    })

  val react: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("react"))
    .withRequest(implicit m => {
      CreateReaction(m.textChannel.id, m.message.id, checkMark)
    })

  val register: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("register"))
    .asyncOpt(implicit m => {
      val playerCreate = PlayerKeeper.PlayerCreate(m.user.id.toUnsignedLong, m.user.username)
      val insertedPlayer = DatabaseService.playerKeeper.insertNewPlayer(playerCreate)
      OptFuture.fromFuture(insertedPlayer).map {
        case Right(_) =>
          val react = CreateReaction(m.textChannel.id, m.message.id, checkMark)
          val respond = m.textChannel.sendMessage(s"Player '${m.user.username}' successfully added")
          client.requestsHelper.run(react).zip(client.requestsHelper.run(respond)).map(_ => ())
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          val react = CreateReaction(m.textChannel.id, m.message.id, negativeMark)
          val respond = m.textChannel.sendMessage(s"Player '${m.user.username}' already exists")
          client.requestsHelper.run(react).zip(client.requestsHelper.run(respond)).map(_ => ())
      }
    })

  def tokenizeCommand(command: String): Array[String] = command.trim.tail.split(' ').tail

  sealed trait Role
  final case object Top extends Role
  final case object Jungle extends Role
  final case object Mid extends Role
  final case object Bot extends Role
  final case object Support extends Role
  final case object Fill extends Role

  def parseRole(role: String): Option[Role] = {
    role.toLowerCase match {
      case "top" => Some(Top)
      case "jungle" => Some(Jungle)
      case "mid" => Some(Mid)
      case "bot" => Some(Bot)
      case "sup" | "support" => Some(Support)
      case "fill" | "any" => Some(Fill)
      case _ => None
    }
  }

  sealed trait QueueError {
    def message: String
  }
  final case object ErrorParsingRole extends QueueError {
    override def message: String = "Error parsing desired roles."
  }
  final case object RoleOrderNotValid extends QueueError {
    override def message: String = "Role order is not valid. 'Fill' must be the only or last chosen role."
  }
  final case object PlayerDoesNotExist extends QueueError {
    override def message: String = "Player was not found. Make sure you register before joining the queue."
  }
  final case object PlayerAlreadyInQueue extends QueueError {
    override def message: String = "Player is already in the queue."
  }

  case class QueuedPlayer(discordId: UserId, roles: Seq[Role])
  var queue: Seq[QueuedPlayer] = Seq.empty

  val join: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("join"))
    .asyncOpt(implicit command => {
      val attributes = tokenizeCommand(command.message.content)
      val parsedRoles: Either[QueueError, Seq[Role]] = attributes.take(2).map(parseRole) match {
        case roles if roles.exists(_.isEmpty) => Left(ErrorParsingRole)
        case roles if roles.isEmpty => Right(Seq(Fill))
        case roles if roles.length == 1 && roles.head.contains(Fill) => Right(Seq(Fill))
        case roles if roles.length == 1 => Right(Seq(roles.head, Some(Fill)).flatten)
        case roles if roles.head.contains(Fill) => Left(RoleOrderNotValid)
        case roles => Right(roles.flatten.toSeq)
      }

      val result: Future[Either[QueueError, QueuedPlayer]] = DatabaseService.playerKeeper.playerExists(command.user.id).map {
        case false =>
          Left(PlayerDoesNotExist)
        case true if queue.exists(_.discordId == command.user.id) =>
          Left(PlayerAlreadyInQueue)
        case true =>
          parsedRoles.map(roles => QueuedPlayer(command.user.id, roles))
      }

      OptFuture.fromFuture(result).map {
        case Left(error) =>
          val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
          val respond = command.textChannel.sendMessage(error.message)
          client.requestsHelper.run(react).zip(client.requestsHelper.run(respond)).map(_ => ())
        case Right(queuedPlayer) =>
          queue = queue.appended(queuedPlayer)
          val react = CreateReaction(command.textChannel.id, command.message.id, checkMark)
          val respond = command.textChannel.sendMessage(s"You have joined the queue with roles: ${queuedPlayer.roles.mkString(", ")}")
          val update = command.textChannel.sendMessage(s"Players in the queue: ${queue.length}")
          client.requestsHelper.runMany(react, respond, update).map(_ => ())
      }
    })

  val leave: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("leave"))
    .asyncOpt(implicit command => {
      queue.find(_.discordId == command.user.id) match {
        case Some(user) =>
          queue = queue.filterNot(_ == user)
          val react = CreateReaction(command.textChannel.id, command.message.id, checkMark)
          val respond = command.textChannel.sendMessage(s"You have left the queue")
          val update = command.textChannel.sendMessage(s"Players in the queue: ${queue.length}")
          client.requestsHelper.runMany(react, respond, update).map(_ => ())
        case None =>
          val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
          val respond = command.textChannel.sendMessage("You are not in the queue")
          client.requestsHelper.run(react).zip(client.requestsHelper.run(respond)).map(_ => ())
      }
    })

  val clear: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("clear"))
    .withRequest(m => {
      AdminService.adminGate(m) {
        queue = Seq.empty
        m.textChannel.sendMessage("Queue has been cleared")
      }
    })

  val show: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("show"))
    .withRequest(m => {
      m.textChannel.sendMessage(s"Queue (${queue.length}): ${queue.mkString(", ")}")
    })

  val shutdown: NamedCommand[NotUsed] = Command
    .named(commandSymbols, Seq("shutdown"))
    .withRequest(m => {
      AdminService.adminGate(m) {
        shutdownHook()
        m.textChannel.sendMessage("Shutting down in 5 seconds...")
      }
    })
}
