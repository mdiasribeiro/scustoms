package com.scustoms

import ackcord.{CacheSnapshot, DiscordClient, OptFuture}
import ackcord.commands.{CommandBuilder, CommandController, GuildMemberCommandMessage, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.data.Permission
import ackcord.syntax.TextChannelSyntax
import akka.NotUsed
import com.scustoms.Emojis.{negativeMark, positiveMark}
import com.scustoms.database.DatabaseService
import com.scustoms.database.keepers.PlayerKeeper
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class AdminCommands(client: DiscordClient, queueService: QueueService, matchService: MatchmakingService) extends CommandController(client.requests) {
  import MessageParser.Auto._
  //import MessageParser.optional
  implicit val c: DiscordClient = client

  val config: Config = ConfigFactory.load()
  val adminCommandSymbols = Seq(config.getString("scustoms.adminCommandSymbol"))

  val clear: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("clear"))
    .asyncOpt(implicit m => {
      queueService.clear()
      DiscordUtils.reactAndRespond(positiveMark, "Queue has been cleared")
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
          if (queueService.add(QueueService.QueuedPlayer(userId, role))) {
            DiscordUtils.reactAndRespond(positiveMark, s"${command.parsed._1} was added to the queue with role: $role")
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

  def playersToStrings(team: Seq[MatchmakingService.MatchPlayer])(implicit c: CacheSnapshot): Seq[String] = {
    team.map {
      case MatchmakingService.MatchPlayer(discordId, role, dbPlayer) =>
        val mention = discordId.resolve.map(_.mention).getOrElse(dbPlayer.discordUsername)
        s"$mention: ${dbPlayer.gameUsername}, role: $role, rating: ${dbPlayer.rating.getRoleRating(role).getConservativeRating}"
    }
  }

  val start: NamedComplexCommand[NotUsed, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("start"))
    .asyncOpt(implicit m => {
      OptFuture.fromFuture(queueService.extendedInfo)
        .map(queuedPlayers => {
          try {
            val startingMatch = matchService.calculateRoleMatch(queuedPlayers)
            queueService.clear()
            val message1 = m.textChannel.sendMessage(playersToStrings(startingMatch.teamA).mkString(s"Team A\n", "\n", "\n"))
            val message2 = m.textChannel.sendMessage(playersToStrings(startingMatch.teamB).mkString(s"Team B\n", "\n", "\n"))
            client.requestsHelper.runMany(message1, message2).map(_ => ())
          } catch {
            case err: Exception =>
              DiscordUtils.reactAndRespond(negativeMark, s"Error: ${err.getMessage}")
          }
        })
    })

  val enrol: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("enrol"))
    .parsing[(String, String)]
    .asyncOpt(implicit m => {
      val result: Either[String, Future[Either[String, String]]] = for {
        targetId <- DiscordUtils.getUserIdFromMention(m.parsed._1).toRight("Mention could not be parsed")
        targetUser <- targetId.resolve.toRight(s"Player ${m.parsed._1} could not be found in this server")
        playerCreate = PlayerKeeper.PlayerCreate(targetId, targetUser.username, m.parsed._2)
      } yield DatabaseService.playerKeeper.insert(playerCreate).map {
        case Right(_) =>
          Right(s"Player '${targetUser.username}' successfully added")
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          Left(s"Player '${targetUser.username}' already exists")
        case Left(_) =>
          Left("Unknown error occurred")
      }
      val flattenedResult: OptFuture[Either[String, String]] = result match {
        case Left(s) => OptFuture.pure(Left(s))
        case Right(f) => OptFuture.fromFuture(f)
      }
      flattenedResult.flatMap {
        case Right(response) =>
          DiscordUtils.reactAndRespond(positiveMark, response)
        case Left(error) =>
          DiscordUtils.reactAndRespond(negativeMark, error)
      }
    })

  val shutdown: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("shutdown"))
    .asyncOpt(implicit m => {
      println("Shutting down in 5 seconds...")
      for {
        _ <- Future {Thread.sleep(5000)}
        _ <- client.shutdownAckCord()
      } yield System.exit(0)
      client.requestsHelper.run(m.textChannel.sendMessage("Shutting down in 5 seconds...")).map(_ => ())
    })

  val commandList = Seq(clear, add, remove, start, enrol, shutdown)
}
