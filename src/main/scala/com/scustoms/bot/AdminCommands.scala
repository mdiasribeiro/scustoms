package com.scustoms.bot

import ackcord.commands._
import ackcord.data.{Permission, UserId}
import ackcord.requests.{AddGuildMemberRole, CreateReaction, ModifyGuildMember, ModifyGuildMemberData}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, JsonSome, OptFuture}
import akka.NotUsed
import Emojis.{negativeMark, positiveMark}
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.StaticReferences
import com.scustoms.database.keepers.PlayerKeeper.PlayerNotFound
import com.scustoms.services.QueueService.QueuedPlayer
import com.scustoms.services.{MatchmakingService, PlayerService, QueueService}
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class AdminCommands(client: DiscordClient,
                    queueService: QueueService,
                    playerService: PlayerService,
                    matchmakingService: MatchmakingService
                   ) extends CommandController(client.requests) {
  implicit val c: DiscordClient = client

  val config: Config = ConfigFactory.load()
  val adminCommandSymbols = Seq(config.getString("scustoms.adminCommandSymbol"))
  val tablePadding: Int = config.getInt("scustoms.tablePadding")

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
    .parsing[(String, Option[String])](MessageParser.Auto.deriveParser)
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

  val start: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("start"))
    .asyncOpt(implicit m => {
      matchmakingService.ongoingMatch match {
        case Some(_) =>
          DiscordUtils.reactAndRespond(negativeMark, s"A match is already on-going. Finish that one before starting another.")
        case None =>
          OptFuture.fromFuture(queueService.extendedInfo).map{
            case Right(queuedPlayers) =>
              try {
                val startingMatch = matchmakingService.calculateRoleMatch(queuedPlayers)
                matchmakingService.ongoingMatch = Some(startingMatch)
                queueService.clear()
                val msg = DiscordUtils.matchToString(startingMatch, "TEAM GEMESES", "TEAM SHIKISHOKU")
                client.requestsHelper.run(m.textChannel.sendMessage(msg)).map(_ => ())
              } catch {
                case err: Exception =>
                  DiscordUtils.reactAndRespond(negativeMark, s"Error: ${err.getMessage}")
              }
            case Left(PlayerNotFound) =>
              m.textChannel.sendMessage(s"Error: A player was not found in the database")
            case Left(_) =>
              m.textChannel.sendMessage(s"An unknown error has occurred")
          }
      }
    })

  val abort: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("abort"))
    .asyncOpt(implicit m => {
      matchmakingService.ongoingMatch match {
        case Some(ongoingMatch) =>
          matchmakingService.ongoingMatch = None
          ongoingMatch.teamA.foreach(p => queueService.add(QueuedPlayer(p.discordId, p.role)))
          ongoingMatch.teamB.foreach(p => queueService.add(QueuedPlayer(p.discordId, p.role)))
          DiscordUtils.reactAndRespond(positiveMark, s"Match aborted. Players were placed back in the queue.")
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no on-going match to abort.")
      }
    })

  val enrol: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("enrol"))
    .parsing[(String, String)](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit m => {
      val result: Either[String, Future[Either[String, (String, UserId)]]] = for {
        targetId <- DiscordUtils.getUserIdFromMention(m.parsed._1).toRight("Mention could not be parsed")
        targetUser <- targetId.resolve.toRight(s"Player ${m.parsed._1} could not be found in this server")
      } yield playerService.insert(targetId, targetUser.username, m.parsed._2).map {
        case Right(_) =>
          Right((s"Player '${targetUser.username}' successfully added", targetId))
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          Left(s"Player '${targetUser.username}' already exists")
        case Left(_) =>
          Left("Unknown error occurred")
      }
      val flattenedResult: OptFuture[Either[String, (String, UserId)]] = result match {
        case Left(s) => OptFuture.pure(Left(s))
        case Right(f) => OptFuture.fromFuture(f)
      }
      flattenedResult.flatMap {
        case Right((response, userId)) =>
          val changeRole = AddGuildMemberRole(m.guild.id, userId, StaticReferences.customsRoleId)
          val react = CreateReaction(m.textChannel.id, m.message.id, positiveMark)
          val respond = m.textChannel.sendMessage(response)
          client.requestsHelper.runMany(react, respond, changeRole).map(_ => ())
        case Left(error) =>
          DiscordUtils.reactAndRespond(negativeMark, error)
      }
    })

  val shutdown: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("shutdown"))
    .parsing[String]
    .asyncOpt(implicit m => {
      println("Shutting down in 5 seconds...")
      for {
        _ <- Future {Thread.sleep(5000)}
        _ <- client.shutdownAckCord()
      } yield System.exit(0)
      client.requestsHelper.run(m.textChannel.sendMessage("Shutting down in 5 seconds...")).map(_ => ())
    })

  val relocate: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq("relocate"))
    .asyncOpt(implicit m => {
      matchmakingService.ongoingMatch match {
        case Some(ongoingMatch) =>
          val moveTeamA = ongoingMatch.teamA.map(player => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamAChannel))
            ModifyGuildMember(m.guild.id, player.discordId, newData)
          })
          val moveTeamB = ongoingMatch.teamB.map(player => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamBChannel))
            ModifyGuildMember(m.guild.id, player.discordId, newData)
          })
          val allPlayers = moveTeamA ++ moveTeamB
          client.requestsHelper.runMany(allPlayers: _*).map(_ => ())
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing match")
      }
    })

  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(adminCommandSymbols, Seq("help"))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(m => {
      val symbolStr = adminCommandSymbols.head
      val helpText = m.parsed.map(_.toLowerCase) match {
        case Some("clear") =>
          s"""```
             |Clear the queue
             |Usage: ${symbolStr}clear
             |```""".stripMargin
        case Some("add") =>
          s"""```
             |Add the target player to the queue
             |Usage: ${symbolStr}add <discord_mention> <?role>
             |(Possible roles: top, jungle, mid, bot, sup/support, fill/any/empty)
             |```""".stripMargin
        case Some("remove") =>
          s"""```
             |Remove the target player from the queue
             |Usage: ${symbolStr}remove <discord_mention>
             |```""".stripMargin
        case Some("start") =>
          s"""```
             |Generate a match with the players currently in the queue
             |Usage: ${symbolStr}start
             |```""".stripMargin
        case Some("abort") =>
          s"""```
             |Abort an ongoing match and place the players back in the queue (with the roles assigned in the match)
             |Usage: ${symbolStr}abort
             |```""".stripMargin
        case Some("enrol") =>
          s"""```
             |Register another user
             |Usage: ${symbolStr}enrol <discord_mention>
             |```""".stripMargin
        case Some("shutdown") =>
          s"""```
             |Shuts the bot down
             |Usage: ${symbolStr}shutdown -a
             |```""".stripMargin
        case Some("relocate") =>
          s"""```
             |Move match players to the right voice rooms
             |Usage: ${symbolStr}relocate
             |```""".stripMargin
        case _ =>
          s"""```Admin command list:
             |Clear     Clear the queue                              (${symbolStr}clear)
             |Add       Add the target player to the queue           (${symbolStr}add <discord_mention> <?role>)
             |Remove    Remove the target player from the queue      (${symbolStr}remove <discord_mention>)
             |Start     Starts a match with the players in the queue (${symbolStr}start)
             |Abort     Abort an ongoing match                       (${symbolStr}abort)
             |Enrol     Register another user                        (${symbolStr}enrol <discord_mention>)
             |Relocate  Move match players to the right voice rooms  (${symbolStr}relocate)
             |Shutdown  Shuts the bot down                           (${symbolStr}shutdown -a)
             |
             |For more details say ${symbolStr}help <command>
             |```""".stripMargin
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(clear, add, remove, start, abort, enrol, shutdown, help, relocate)
}
