package com.scustoms

import ackcord.{DiscordClient, OptFuture}
import ackcord.commands.{CommandBuilder, CommandController, GuildMemberCommandMessage, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.data.Permission
import ackcord.requests.CreateReaction
import akka.NotUsed
import ackcord.syntax.TextChannelSyntax
import com.scustoms.Emojis.{positiveMark, negativeMark}
import com.scustoms.QueueService._
import com.scustoms.database.DatabaseService
import com.scustoms.database.keepers.PlayerKeeper
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future
import scala.util.Random

class BotCommands(client: DiscordClient, shutdownHook: () => Unit) extends CommandController(client.requests) {
  import MessageParser.Auto._
  implicit val c: DiscordClient = client

  val config: Config = ConfigFactory.load()
  val userCommandSymbols = Seq(config.getString("scustoms.userCommandSymbol"))
  val adminCommandSymbols = Seq(config.getString("scustoms.adminCommandSymbol"))

  val queueService = new QueueService

  val randomGenerator = new Random()

  val hello: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("hello"))
    .withRequest(m => m.textChannel.sendMessage(s"Hello ${m.user.username}!"))

  val status: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("status"))
    .withRequest(m => {
      m.textChannel.sendMessage(s"I'm currently running version '0.03' on 'docker-compose-host'")
    })

  val info: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("info"))
    .asyncOpt(implicit userCommandMessage => {
      val findPlayerFuture = OptFuture.fromFuture(DatabaseService.playerKeeper.find(userCommandMessage.user.id))
      findPlayerFuture.flatMap {
        case Some(player) =>
          val react = CreateReaction(userCommandMessage.textChannel.id, userCommandMessage.message.id, positiveMark)
          val message =
            s"""${userCommandMessage.user.mention}
              |In-game name: ${player.gameUsername}
              |Conservative rating: ${player.rating.getConservativeRating}
              |Mean: ${player.rating.getMean}
              |Std deviation: ${player.rating.getStandardDeviation}""".stripMargin
          val respond = userCommandMessage.textChannel.sendMessage(message)
          client.requestsHelper.runMany(react, respond).map(_ => ())
        case None =>
          val react = CreateReaction(userCommandMessage.textChannel.id, userCommandMessage.message.id, negativeMark)
          val message = "Player not found. Make sure you have registered first."
          val respond = userCommandMessage.textChannel.sendMessage(message)
          client.requestsHelper.runMany(react, respond).map(_ => ())
      }
    })

  val react: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("react"))
    .asyncOpt(implicit userCommandMessage => {
      val react = CreateReaction(userCommandMessage.textChannel.id, userCommandMessage.message.id, positiveMark)
      client.requestsHelper.run(react).map(_ => ())
    })

  val register: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("register"))
    .asyncOpt(implicit m => {
      val playerCreate = PlayerKeeper.PlayerCreate(m.user.id, m.user.username, m.user.username)
      val insertedPlayer = DatabaseService.playerKeeper.insert(playerCreate)
      OptFuture.fromFuture(insertedPlayer).map {
        case Right(_) =>
          val react = CreateReaction(m.textChannel.id, m.message.id, positiveMark)
          val respond = m.textChannel.sendMessage(s"Player '${m.user.username}' successfully added")
          client.requestsHelper.runMany(react, respond).map(_ => ())
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          val react = CreateReaction(m.textChannel.id, m.message.id, negativeMark)
          val respond = m.textChannel.sendMessage(s"Player '${m.user.username}' already exists")
          client.requestsHelper.runMany(react, respond).map(_ => ())
      }
    })

  val join: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("join"))
    .parsing[Option[String]]
    .asyncOpt(implicit command => {
      println(command.parsed)
      val parsedRole: Either[QueueError, Role] = QueueService.parseRole(command.parsed.getOrElse("fill")).toRight(ErrorParsingRole)

      val result: Future[Either[QueueError, QueuedPlayer]] = DatabaseService.playerKeeper.exists(command.user.id).map {
        case false =>
          Left(PlayerDoesNotExist)
        case true if queueService.contains(command.user.id) =>
          Left(PlayerAlreadyInQueue)
        case true =>
          parsedRole.map(role => QueuedPlayer(command.user.id, role))
      }

      OptFuture.fromFuture(result).map {
        case Left(error) =>
          val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
          val respond = command.textChannel.sendMessage(error.message)
          client.requestsHelper.runMany(react, respond)
        case Right(queuedPlayer) =>
          queueService.add(queuedPlayer)
          val react = CreateReaction(command.textChannel.id, command.message.id, positiveMark)
          val message = s"${command.user.mention} joined the queue (role: ${queuedPlayer.role}). Current queue size: ${queueService.length}"
          val respond = command.textChannel.sendMessage(message)
          client.requestsHelper.runMany(react, respond)
      }
    })

  val leave: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("leave"))
    .asyncOpt(implicit command => {
      if (queueService.remove(command.user.id)) {
        val react = CreateReaction(command.textChannel.id, command.message.id, positiveMark)
        val respond = command.textChannel.sendMessage(s"${command.user.mention} left the queue. Current queue size: ${queueService.length}")
        client.requestsHelper.runMany(react, respond).map(_ => ())
      } else {
        val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
        val respond = command.textChannel.sendMessage("You are not in the queue")
        client.requestsHelper.runMany(react, respond).map(_ => ())
      }
    })

  val show: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("show"))
    .asyncOptRequest(implicit m =>
      OptFuture.fromFuture(queueService.extendedInfo)
        .map(allPlayers => {
          val niceString = allPlayers.map {
            case ExtendedQueuedPlayer(_, role, dbPlayer) =>
              s"username: ${dbPlayer.gameUsername}, role: $role, rating: ${dbPlayer.rating.getConservativeRating}"
          }.mkString(s"Queue (${allPlayers.length})\n", "\n", "\n")
          m.textChannel.sendMessage(niceString)
        })
    )

  val clear: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("clear"))
    .asyncOpt(implicit m => {
      queueService.clear()
      client.requestsHelper.run(m.textChannel.sendMessage("Queue has been cleared")).map(_ => ())
    })

  val add: NamedComplexCommand[(String, Option[String]), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("add"))
    .parsing[(String, Option[String])]
    .asyncOpt(implicit command => {
      val parsedUserId = DiscordUtils.getUserIdFromMention(command.parsed._1)
      val parsedRole = QueueService.parseRole(command.parsed._2.getOrElse("fill"))
      (parsedUserId, parsedRole) match {
        case (Some(userId), Some(role)) =>
          println(s"detected mention id: $userId")
          if (queueService.add(QueueService.QueuedPlayer(userId, role))) {
            val react = CreateReaction(command.textChannel.id, command.message.id, positiveMark)
            val respond = command.textChannel.sendMessage(s"${command.parsed} was added to the queue")
            client.requestsHelper.runMany(react, respond).map(_ => ())
          } else {
            val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
            val respond = command.textChannel.sendMessage(s"${command.parsed} is already in the queue")
            client.requestsHelper.runMany(react, respond).map(_ => ())
          }
        case (Some(userId), None) =>
          println(s"detected mention id: $userId")
          val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
          val respond = command.textChannel.sendMessage("Player role could not be parsed")
          client.requestsHelper.runMany(react, respond).map(_ => ())
        case (None, _) =>
          val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
          val respond = command.textChannel.sendMessage("Mention could not be parsed")
          client.requestsHelper.runMany(react, respond).map(_ => ())
      }
    })

  val remove: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("remove"))
    .parsing[String]
    .asyncOpt(implicit command => {
      DiscordUtils.getUserIdFromMention(command.parsed) match {
        case Some(userId) =>
          println(s"detected mention id: $userId")
          if (queueService.remove(userId)) {
            val react = CreateReaction(command.textChannel.id, command.message.id, positiveMark)
            val respond = command.textChannel.sendMessage(s"${command.parsed} was removed from the queue")
            client.requestsHelper.runMany(react, respond).map(_ => ())
          } else {
            val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
            val respond = command.textChannel.sendMessage("Player was not found in the queue")
            client.requestsHelper.runMany(react, respond).map(_ => ())
          }
        case None =>
          val react = CreateReaction(command.textChannel.id, command.message.id, negativeMark)
          val respond = command.textChannel.sendMessage("Mention could not be parsed")
          client.requestsHelper.runMany(react, respond).map(_ => ())
      }
    })

  val start: NamedComplexCommand[NotUsed, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("start"))
    .asyncOpt(implicit m => {
      OptFuture.fromFuture(queueService.extendedInfo)
        .map(queuedPlayers => {
          queueService.clear()
          val (teamA, teamB) = randomGenerator
            .shuffle(queuedPlayers)
            .map {
              case ExtendedQueuedPlayer(discordId, role, dbPlayer) =>
                val mention = discordId.resolve.map(_.mention).getOrElse(dbPlayer.discordUsername)
                s"$mention - username: ${dbPlayer.gameUsername}, role: $role, rating: ${dbPlayer.rating.getConservativeRating}"
            }
            .splitAt(queuedPlayers.length / 2)
          val message1 = m.textChannel.sendMessage(teamA.mkString(s"Team A\n", "\n", "\n"))
          val message2 = m.textChannel.sendMessage(teamB.mkString(s"Team B\n", "\n", "\n"))
          client.requestsHelper.runMany(message1, message2).map(_ => ())
        })
    })

  val shutdown: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("shutdown"))
    .asyncOpt(implicit m => {
      shutdownHook()
      client.requestsHelper.run(m.textChannel.sendMessage("Shutting down in 5 seconds...")).map(_ => ())
    })
}
