package com.scustoms.bot

import ackcord.commands._
import ackcord.data.{Permission, UserId}
import ackcord.requests.{AddGuildMemberRole, CreateReaction, ModifyGuildMember, ModifyGuildMemberData}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, JsonSome, OptFuture}
import akka.NotUsed
import Emojis.{negativeMark, positiveMark}
import com.scustoms.Utils
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.StaticReferences
import com.scustoms.database.keepers.PlayerKeeper.PlayerNotFound
import com.scustoms.services.MatchmakingService.{CompleteMatch, MatchPlayer, MatchTeam, OngoingMatch}
import com.scustoms.services.QueueService.QueuedPlayer
import com.scustoms.services.{MatchService, MatchmakingService, PlayerService, QueueService, RatingService}
import com.scustoms.trueskill.TwoTeamCalculator
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class AdminCommands(client: DiscordClient,
                    queueService: QueueService,
                    playerService: PlayerService,
                    matchmakingService: MatchmakingService,
                    matchService: MatchService
                   ) extends CommandController(client.requests) {
  implicit val c: DiscordClient = client

  val config: Config = ConfigFactory.load()
  val adminCommandSymbols = Seq(config.getString("scustoms.adminCommandSymbol"))
  val tablePadding: Int = config.getInt("scustoms.tablePadding")

  final val ClearString = "clearQueue"
  val clear: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(ClearString))
    .asyncOpt(implicit m => {
      queueService.clear()
      DiscordUtils.reactAndRespond(positiveMark, "Queue has been cleared")
    })

  final val AddString = "AddPlayer"
  val add: NamedComplexCommand[(String, Option[String]), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(AddString))
    .parsing[(String, Option[String])](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit command => {
      val parsedUserId = DiscordUtils.getUserIdFromMention(command.parsed._1)
      val parsedRole = command.parsed._2.map(QueueService.parseRole)
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

  final val RemoveString = "RemovePlayer"
  val remove: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(RemoveString))
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

  final val StartString = "startMatch"
  val start: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(StartString))
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
                val msg = DiscordUtils.ongoingMatchToString(startingMatch, "TEAM GEMESES", "TEAM SHIKISHOKU")
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

  final val AbortString = "abortMatch"
  val abort: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(AbortString))
    .asyncOpt(implicit m => {
      matchmakingService.ongoingMatch match {
        case Some(ongoingMatch) =>
          matchmakingService.ongoingMatch = None
          ongoingMatch.team1.seq.foreach(p => queueService.add(QueuedPlayer(p.discordId, Some(p.role))))
          ongoingMatch.team2.seq.foreach(p => queueService.add(QueuedPlayer(p.discordId, Some(p.role))))
          DiscordUtils.reactAndRespond(positiveMark, s"Match aborted. Players were placed back in the queue.")
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no on-going match to abort.")
      }
    })

  final val EnrolString = "enrolPlayer"
  val enrol: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(EnrolString))
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

  final val ShutdownString = "shutdown"
  val shutdown: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(ShutdownString))
    .parsing[String]
    .asyncOpt(implicit m => {
      println("Shutting down in 3 seconds...")
      for {
        _ <- Future {Thread.sleep(3000)}
        _ <- client.shutdownAckCord()
      } yield System.exit(0)
      client.requestsHelper.run(m.textChannel.sendMessage("Shutting down in 3 seconds...")).map(_ => ())
    })

  final val WinnerString = "declareWinner"
  val winner: NamedComplexCommand[Int, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(WinnerString))
    .parsing[Int]
    .asyncOpt(implicit m => {
      val winningTeam = m.parsed match {
        case 1 => Some(true)
        case 2 => Some(false)
        case _ => None
      }
      (winningTeam, matchmakingService.ongoingMatch) match {
        case (_, None) =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing match.")
        case (None, _) =>
          DiscordUtils.reactAndRespond(negativeMark, s"Team number is not valid. Must be 1 or 2.")
        case (Some(team1Won), Some(ongoingMatch)) =>
          matchmakingService.ongoingMatch = None
          val completeMatch = RatingService.calculate(ongoingMatch, team1Won)
          matchService.insert(completeMatch)
          DiscordUtils.reactAndRespond(positiveMark, s"Game result has been saved and player ratings updated.")
      }
    })

  final val ResetString = "resetRatings"
  val reset: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(ResetString))
    .asyncOpt(implicit m => OptFuture
      .fromFuture(playerService.resetRating)
      .map(_ => DiscordUtils.reactAndRespond(positiveMark, s"Database ratings have been reset"))
    )

  final val ReprocessString = "reprocessMatches"
  val reprocess: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(ReprocessString))
    .asyncOpt(implicit m => {

      def completeMatchToOngoingMatch(m: CompleteMatch): Future[Either[DatabaseError, OngoingMatch]] = {
        for {
          resolvedTeamA <- Future.sequence(m.teamA.map(playerService.resolvePlayer))
          resolvedTeamB <- Future.sequence(m.teamB.map(playerService.resolvePlayer))
        } yield {
          if (resolvedTeamA.contains(None) || resolvedTeamB.contains(None)) {
            Left(PlayerKeeper.PlayerNotFound)
          } else {
            val teamA = MatchTeam.fromPlayersWithStatistics(resolvedTeamA.flatten)
            val teamB = MatchTeam.fromPlayersWithStatistics(resolvedTeamB.flatten)
            val score = TwoTeamCalculator.calculateMatchQuality(RatingService.defaultGameInfo, teamA, teamB)
            Right(OngoingMatch(score, teamA, teamB))
          }
        }
      }

      def convertMatches(m: Seq[CompleteMatch]): Future[Either[DatabaseError, Seq[OngoingMatch]]] = {
        for {
          seqProcessedMatches <- Future.sequence(m.map(completeMatchToOngoingMatch))
        } yield Utils.sequenceEither(seqProcessedMatches)
      }

      val limit = 100
      var offset = 0
      var hasMore = true
      while (hasMore) {
        for {
          batchEither <- matchService.get(limit, offset)
          batch <- batchEither
          stillHasMore = batch.length >= limit
          newOffset = offset + batch.length
          convertedEither <- convertMatches(batch)
          ongoingMatches <- convertedEither
        } yield {
          offset = newOffset
          hasMore = stillHasMore
          ongoingMatches.map(RatingService.calculate(_, true))
        }

      }
    })

  final val RelocateString = "relocateVoice"
  val relocate: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(RelocateString))
    .asyncOpt(implicit m => {
      matchmakingService.ongoingMatch match {
        case Some(ongoingMatch) =>
          val moveTeamA = ongoingMatch.team1.seq.map(player => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamAChannel))
            ModifyGuildMember(m.guild.id, player.discordId, newData)
          })
          val moveTeamB = ongoingMatch.team2.seq.map(player => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamBChannel))
            ModifyGuildMember(m.guild.id, player.discordId, newData)
          })
          val allPlayers = moveTeamA ++ moveTeamB
          client.requestsHelper.runMany(allPlayers: _*).map(_ => ())
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing match")
      }
    })

  val helpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(adminCommandSymbols, Seq(helpString))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(m => {
      val symbolStr = adminCommandSymbols.head
      val helpText = m.parsed.map(_.toLowerCase) match {
        case Some(ClearString) =>
          s"""```
             |Clear the queue
             |Usage: $symbolStr$ClearString
             |```""".stripMargin
        case Some(AddString) =>
          s"""```
             |Add the target player to the queue
             |Usage: $symbolStr$AddString <discord_mention> <?role>
             |(Possible roles: top, jungle, mid, bot, sup/support, fill/any/empty)
             |```""".stripMargin
        case Some(RemoveString) =>
          s"""```
             |Remove the target player from the queue
             |Usage: $symbolStr$RemoveString <discord_mention>
             |```""".stripMargin
        case Some(StartString) =>
          s"""```
             |Generate a match with the players currently in the queue
             |Usage: $symbolStr$StartString
             |```""".stripMargin
        case Some(AbortString) =>
          s"""```
             |Abort an ongoing match and place the players back in the queue (with the roles assigned in the match)
             |Usage: $symbolStr$AbortString
             |```""".stripMargin
        case Some(EnrolString) =>
          s"""```
             |Register another user
             |Usage: $symbolStr$EnrolString <discord_mention>
             |```""".stripMargin
        case Some(ShutdownString) =>
          s"""```
             |Shuts the bot down
             |Usage: $symbolStr$ShutdownString -a
             |```""".stripMargin
        case Some(WinnerString) =>
          s"""```
             |Declare a winner for an on-going match
             |Usage: $symbolStr$WinnerString <winning_team>
             |Winning team parameter should be either the integer 1 or 2
             |```""".stripMargin
        case Some(ResetString) =>
          s"""```
             |Reset all players rating to default values
             |Usage: $symbolStr$ResetString
             |```""".stripMargin
        case Some(RelocateString) =>
          s"""```
             |Move match players to the right voice rooms
             |Usage: $symbolStr$RelocateString
             |```""".stripMargin
        case _ =>
          val clear = s"$symbolStr$ClearString".padTo(tablePadding, ' ')
          val add = s"$symbolStr$AddString".padTo(tablePadding, ' ')
          val remove = s"$symbolStr$RemoveString".padTo(tablePadding, ' ')
          val start = s"$symbolStr$StartString".padTo(tablePadding, ' ')
          val abort = s"$symbolStr$AbortString".padTo(tablePadding, ' ')
          val enrol = s"$symbolStr$EnrolString".padTo(tablePadding, ' ')
          val winner = s"$symbolStr$WinnerString".padTo(tablePadding, ' ')
          val reset = s"$symbolStr$ResetString".padTo(tablePadding, ' ')
          val relocate = s"$symbolStr$RelocateString".padTo(tablePadding, ' ')
          val shutdown = s"$symbolStr$ShutdownString".padTo(tablePadding, ' ')
          Seq(
            s"""$clear Clear the queue""",
            s"""$add Add the target player to the queue""",
            s"""$remove Remove the target player from the queue""",
            s"""$start Starts a match with the players in the queue""",
            s"""$abort Abort an ongoing match""",
            s"""$enrol Register another user""",
            s"""$winner Declare a winner for an on-going match""",
            s"""$reset Reset all players rating to default values""",
            s"""$relocate Move match players to the right voice rooms""",
            s"""$shutdown Shuts the bot down"""
          ).mkString("```Admin command list:\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(clear, add, remove, start, abort, enrol, shutdown, help, winner, reset, relocate)
}
