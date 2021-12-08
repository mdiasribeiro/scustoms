package com.scustoms

import ackcord.{DiscordClient, OptFuture}
import ackcord.commands.{CommandBuilder, CommandController, GuildMemberCommandMessage, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.data.Permission
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

  val info: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("info"))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(DatabaseService.playerKeeper.find(userCommandMessage.user.id)).flatMap {
        case Some(player) =>
          val message =
            s"""${userCommandMessage.user.mention}
              |In-game name: ${player.gameUsername}
              |Conservative rating: ${player.rating.getConservativeRating}
              |Mean: ${player.rating.getMean}
              |Std deviation: ${player.rating.getStandardDeviation}""".stripMargin
          DiscordUtils.reactAndRespond(positiveMark, message)
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Player not found. Make sure you have registered first.")
      }
    )

  val register: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("register"))
    .asyncOpt(implicit m => {
      val playerCreate = PlayerKeeper.PlayerCreate(m.user.id, m.user.username, m.user.username)
      OptFuture.fromFuture(DatabaseService.playerKeeper.insert(playerCreate)).map {
        case Right(_) =>
          DiscordUtils.reactAndRespond(positiveMark, s"Player '${m.user.username}' successfully added")
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          DiscordUtils.reactAndRespond(negativeMark, s"Player '${m.user.username}' already exists")
      }
    })

  val join: NamedComplexCommand[String, NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("join"))
    .parsing[String]
    .asyncOpt(implicit command => {
      val result: Future[Either[QueueError, QueuedPlayer]] = DatabaseService.playerKeeper.exists(command.user.id).map {
        case false =>
          Left(PlayerDoesNotExist)
        case true if queueService.contains(command.user.id) =>
          Left(PlayerAlreadyInQueue)
        case true =>
          QueueService
            .parseRole(command.parsed)
            .toRight(ErrorParsingRole)
            .map(role => QueuedPlayer(command.user.id, role))
      }

      OptFuture.fromFuture(result).map {
        case Right(queuedPlayer) =>
          queueService.add(queuedPlayer)
          val message = s"${command.user.mention} joined the queue (role: ${queuedPlayer.role}). Current queue size: ${queueService.length}"
          DiscordUtils.reactAndRespond(positiveMark, message)
        case Left(error) =>
          DiscordUtils.reactAndRespond(negativeMark, error.message)
      }
    })

  val leave: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("leave"))
    .asyncOpt(implicit command =>
      if (queueService.remove(command.user.id)) {
        DiscordUtils.reactAndRespond(positiveMark, s"${command.user.mention} left the queue. Current queue size: ${queueService.length}")
      } else {
        DiscordUtils.reactAndRespond(negativeMark, "You are not in the queue")
      }
    )

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
      DiscordUtils.reactAndRespond(positiveMark, "Queue has been cleared")
    })

  val add: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("add"))
    .parsing[(String, String)]
    .asyncOpt(implicit command => {
      val parsedUserId = DiscordUtils.getUserIdFromMention(command.parsed._1)
      val parsedRole = QueueService.parseRole(command.parsed._2)
      (parsedUserId, parsedRole) match {
        case (Some(userId), Some(role)) =>
          if (queueService.add(QueueService.QueuedPlayer(userId, role))) {
            DiscordUtils.reactAndRespond(positiveMark, s"${command.parsed._1} was added to the queue with role: ${command.parsed._2}")
          } else {
            DiscordUtils.reactAndRespond(negativeMark, s"${command.parsed._1} is already in the queue")
          }
        case (Some(_), None) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player role could not be parsed")
        case (None, _) =>
          DiscordUtils.reactAndRespond(negativeMark, "Mention could not be parsed")
      }
    })

  val remove: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("remove"))
    .parsing[String]
    .asyncOpt(implicit command => {
      DiscordUtils.getUserIdFromMention(command.parsed).map(queueService.remove) match {
        case Some(true) =>
          DiscordUtils.reactAndRespond(positiveMark, s"${command.parsed} was removed from the queue")
        case Some(false) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player was not found in the queue")
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Mention could not be parsed")
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
