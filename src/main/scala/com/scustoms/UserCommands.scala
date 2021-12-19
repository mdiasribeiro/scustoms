package com.scustoms

import ackcord.{DiscordClient, OptFuture}
import ackcord.commands.{CommandController, MessageParser, NamedCommand, NamedComplexCommand}
import akka.NotUsed
import ackcord.syntax.TextChannelSyntax
import com.scustoms.Emojis.{negativeMark, positiveMark}
import com.scustoms.QueueService._
import com.scustoms.database.DatabaseService
import com.scustoms.database.keepers.PlayerKeeper
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class UserCommands(client: DiscordClient, queueService: QueueService, matchService: MatchmakingService) extends CommandController(client.requests) {
  import MessageParser.optional
  implicit val c: DiscordClient = client

  val config: Config = ConfigFactory.load()
  val userCommandSymbols = Seq(config.getString("scustoms.userCommandSymbol"))

  val hello: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("hello"))
    .withRequest(m => m.textChannel.sendMessage(s"Hello ${m.user.username}!"))

  val info: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("info"))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(DatabaseService.playerKeeper.find(userCommandMessage.user.id)).flatMap {
        case Some(player) =>
          val topRating = player.rating.top
          val jungleRating = player.rating.jungle
          val midRating = player.rating.mid
          val botRating = player.rating.bot
          val supportRating = player.rating.support
          val message =
            s"""${userCommandMessage.user.mention}
              |In-game name: ${player.gameUsername}
              |Top: Conservative rating ${topRating.getConservativeRating} - Mean ${topRating.getMean} - Std deviation ${topRating.getStandardDeviation}
              |Jungle: Conservative rating ${jungleRating.getConservativeRating} - Mean ${jungleRating.getMean} - Std deviation ${jungleRating.getStandardDeviation}
              |Mid: Conservative rating ${midRating.getConservativeRating} - Mean ${midRating.getMean} - Std deviation ${midRating.getStandardDeviation}
              |Bot: Conservative rating ${botRating.getConservativeRating} - Mean ${botRating.getMean} - Std deviation ${botRating.getStandardDeviation}
              |Support: Conservative rating ${supportRating.getConservativeRating} - Mean ${supportRating.getMean} - Std deviation ${supportRating.getStandardDeviation}""".stripMargin
          DiscordUtils.reactAndRespond(positiveMark, message)
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Player not found. Make sure you have registered first.")
      }
    )

  val register: NamedComplexCommand[String, NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("register"))
    .parsing[String]
    .asyncOpt(implicit m => {
      val playerCreate = PlayerKeeper.PlayerCreate(m.user.id, m.user.username, m.parsed)
      OptFuture.fromFuture(DatabaseService.playerKeeper.insert(playerCreate)).map {
        case Right(_) =>
          DiscordUtils.reactAndRespond(positiveMark, s"Player '${m.user.username}' successfully added")
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          DiscordUtils.reactAndRespond(negativeMark, s"Player '${m.user.username}' already exists")
      }
    })

  val join: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("join"))
    .parsing[Option[String]]
    .asyncOpt(implicit command => {
      val result: Future[Either[QueueError, QueuedPlayer]] = DatabaseService.playerKeeper.exists(command.user.id).map {
        case false =>
          Left(PlayerDoesNotExist)
        case true if queueService.contains(command.user.id) =>
          Left(PlayerAlreadyInQueue)
        case true =>
          QueueService
            .parseRole(command.parsed.getOrElse("fill"))
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
              s"username: ${dbPlayer.gameUsername}, role: $role, rating: ${dbPlayer.rating.getRoleRating(role).getConservativeRating}"
          }.mkString(s"Queue (${allPlayers.length})\n", "\n", "\n")
          m.textChannel.sendMessage(niceString)
        })
    )

  val commandList = Seq(hello, info, register, join, leave, show)
}
