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
import com.scustoms.database.keepers.MatchKeeper.{StoredMatch, StoredMatchTeam}
import com.scustoms.services.MatchService.OngoingMatch
import com.scustoms.services.QueueService.QueuedPlayer
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.scustoms.trueskill.RatingUtils
import com.typesafe.config.Config

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class AdminCommands(config: Config,
                    queueService: QueueService,
                    playerService: PlayerService,
                    matchService: MatchService
                   )(implicit client: DiscordClient) extends CommandController(client.requests) {

  import com.scustoms.Utils.StringImprovements

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

  final val AddPlayerString = "addPlayer"
  val addPlayer: NamedComplexCommand[(String, Option[String]), NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(AddPlayerString))
    .parsing[(String, Option[String])](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit command => {
      val parsedUserId = DiscordUtils.getUserIdFromMention(command.parsed._1)
      val parsedRole = command.parsed._2.map(QueueService.parseRole).getOrElse(Some(QueueService.Fill))
      (parsedUserId, parsedRole) match {
        case (Some(userId), _) if queueService.contains(userId) =>
          DiscordUtils.reactAndRespond(negativeMark, s"${command.parsed._1} is already in the queue")
        case (Some(userId), _) if matchService.contains(userId) =>
          DiscordUtils.reactAndRespond(negativeMark, s"${command.parsed._1} is already in a match")
        case (Some(userId), Some(role)) =>
          OptFuture.fromFuture(playerService.find(userId)).map {
            case Some(player) =>
              queueService.addPlayer(QueueService.QueuedPlayer(role, player))
              DiscordUtils.reactAndRespond(positiveMark, s"${command.parsed._1} was added to the queue with role: $role")
            case None =>
              DiscordUtils.reactAndRespond(negativeMark, s"${command.parsed._1} could not be found. Make sure he is registered.")
          }
        case (Some(_), None) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player role could not be parsed")
        case (None, _) =>
          DiscordUtils.reactAndRespond(negativeMark, "Mention could not be parsed")
      }
    })

  final val RemoveString = "removePlayer"
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
      matchService.ongoingMatch match {
        case Some(_) =>
          DiscordUtils.reactAndRespond(negativeMark, s"A match is already on-going. Finish that one before starting another.")
        case None =>
          try {
            val startingMatch = RatingUtils.calculateRoleMatch(queueService.getQueue)
            matchService.ongoingMatch = Some(startingMatch)
            queueService.clear()
            val msg = DiscordUtils.ongoingMatchToString(startingMatch, "TEAM 1", "TEAM 2", tablePadding)
            client.requestsHelper.run(m.textChannel.sendMessage(msg)).map(_ => ())
          } catch {
            case err: Exception =>
              DiscordUtils.reactAndRespond(negativeMark, s"Error: ${err.getMessage}")
          }
      }
    })

  final val AbortString = "abortMatch"
  val abort: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(AbortString))
    .asyncOpt(implicit m => {
      matchService.ongoingMatch match {
        case Some(ongoingMatch) =>
          matchService.ongoingMatch = None
          ongoingMatch.team1.seq.foreach(p => queueService.addPlayer(QueuedPlayer(p.role.toQueueRole, p.state)))
          ongoingMatch.team2.seq.foreach(p => queueService.addPlayer(QueuedPlayer(p.role.toQueueRole, p.state)))
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
      val winningTeam = DiscordUtils.parseWinningTeamA(m.parsed)
      (winningTeam, matchService.ongoingMatch) match {
        case (_, None) =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing match.")
        case (None, _) =>
          DiscordUtils.reactAndRespond(negativeMark, "Team number is not valid. Must be 1 or 2.")
        case (Some(team1Won), Some(ongoingMatch)) =>
          matchService.ongoingMatch = None
          val completeMatch = RatingUtils.calculate(ongoingMatch, team1Won)
          val update = matchService.insertAndUpdate(completeMatch)
          OptFuture.fromFuture(update)
            .map(_ => DiscordUtils.reactAndRespond(positiveMark, "Game result has been saved and player ratings updated."))
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
      val limit = 100
      var offset = 0
      var hasMore = true
      Await.result(playerService.resetRating, 10.seconds)
      println("Ratings have been reset.")
      while (hasMore) {
        matchService
          .getUnresolved(limit, offset)
          .map(batch => {
            println(s"Processing a new match batch of ${batch.length} matches...")
            hasMore = batch.length >= limit
            offset = offset + batch.length
            batch.foreach(unresolvedMatch => {
              val processedMatch = matchService.resolveMatch(unresolvedMatch).flatMap {
                case Some(resolvedMatch) =>
                  val ongoingMatch = OngoingMatch.fromResolvedMatch(resolvedMatch)
                  val completeMatch = RatingUtils.calculate(ongoingMatch, resolvedMatch.team1Won)
                  matchService.updateRatings(completeMatch)
                case None =>
                  Future.successful(())
              }
              Await.result(processedMatch, 10.seconds)
            })
            println(s"Batch processed.")
          })
      }
      DiscordUtils.reactAndRespond(positiveMark, s"Match re-processing complete!")
    })

  final val AddMatchString = "addMatch"
  case class AddMatchParams(t1: String, j1: String, m1: String, b1: String, s1: String,
                            t2: String, j2: String, m2: String, b2: String, s2: String, winningTeam: Int)
  def resolveAddMatch(m: AddMatchParams): Option[StoredMatch] =
    for {
      t1 <- DiscordUtils.getUserIdFromMention(m.t1)
      j1 <- DiscordUtils.getUserIdFromMention(m.j1)
      m1 <- DiscordUtils.getUserIdFromMention(m.m1)
      b1 <- DiscordUtils.getUserIdFromMention(m.b1)
      s1 <- DiscordUtils.getUserIdFromMention(m.s1)
      t2 <- DiscordUtils.getUserIdFromMention(m.t2)
      j2 <- DiscordUtils.getUserIdFromMention(m.j2)
      m2 <- DiscordUtils.getUserIdFromMention(m.m2)
      b2 <- DiscordUtils.getUserIdFromMention(m.b2)
      s2 <- DiscordUtils.getUserIdFromMention(m.s2)
      teamAWon <- DiscordUtils.parseWinningTeamA(m.winningTeam)
    } yield StoredMatch(
      0L,
      StoredMatchTeam(t1, j1, m1, b1, s1),
      StoredMatchTeam(t2, j2, m2, b2, s2),
      teamAWon
    )

  val addMatch: NamedComplexCommand[AddMatchParams, NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(AddMatchString))
    .parsing[AddMatchParams](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit m => {
      resolveAddMatch(m.parsed) match {
        case Some(params) =>
          OptFuture.fromFuture(matchService.resolveMatch(params)).map {
            case Some(resolvedMatch) =>
              val ongoingMatch = OngoingMatch.fromResolvedMatch(resolvedMatch)
              val completeMatch = RatingUtils.calculate(ongoingMatch, resolvedMatch.team1Won)
              matchService
                .insertAndUpdate(completeMatch)
                .map(_ => DiscordUtils.reactAndRespond(positiveMark, "Game result has been saved and player ratings updated."))
            case None =>
              DiscordUtils.reactAndRespond(negativeMark, "A player could not be resolved, make sure everyone is registered.")
          }
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Error parsing parameters, make sure mentions and teamAWon value are correct.")
      }
    })

  final val RelocateRoomsString = "relocateRooms"
  val relocateRooms: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(RelocateRoomsString))
    .asyncOpt(implicit m => {
      matchService.ongoingMatch match {
        case Some(ongoingMatch) =>
          val moveTeamA = ongoingMatch.team1.seq.map(player => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamAChannel))
            ModifyGuildMember(m.guild.id, player.state.discordId, newData)
          })
          val moveTeamB = ongoingMatch.team2.seq.map(player => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamBChannel))
            ModifyGuildMember(m.guild.id, player.state.discordId, newData)
          })
          val moveWatchers = queueService.getWatchers.map(watcher => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.teamAChannel))
            ModifyGuildMember(m.guild.id, watcher, newData)
          })
          val allPlayers = moveTeamA ++ moveTeamB ++ moveWatchers
          client.requestsHelper.runMany(allPlayers: _*).map(_ => ())
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "There is no ongoing match")
      }
    })

  final val RelocateLobbyString = "relocateLobby"
  val relocateLobby: NamedCommand[NotUsed] = GuildCommand
    .andThen(CommandBuilder.needPermission[GuildMemberCommandMessage](Permission.Administrator))
    .named(adminCommandSymbols, Seq(RelocateLobbyString))
    .asyncOpt(implicit m => {
      matchService.ongoingMatch match {
        case Some(ongoingMatch) =>
          val allMembers = queueService.getWatchers ++
            ongoingMatch.team1.seq.map(_.state.discordId) ++
            ongoingMatch.team2.seq.map(_.state.discordId)
          val moveTeams = allMembers.map(playerId => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.lobbyChannel))
            ModifyGuildMember(m.guild.id, playerId, newData)
          })
          client.requestsHelper.runMany(moveTeams: _*).map(_ => ())
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "There is no ongoing match")
      }
    })

  val helpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(adminCommandSymbols, Seq(helpString))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(m => {
      val symbolStr = adminCommandSymbols.head
      val helpText = m.parsed match {
        case Some(ClearString) =>
          s"""```
             |Clear the queue
             |Usage: $symbolStr$ClearString
             |```""".stripMargin
        case Some(AddPlayerString) =>
          s"""```
             |Add the target player to the queue
             |Usage: $symbolStr$AddPlayerString <discord_mention> <?role>
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
        case Some(ReprocessString) =>
          s"""```
             |Resets the ratings and reprocess all matches in the database
             |Usage: $symbolStr$ReprocessString
             |```""".stripMargin
        case Some(AddMatchString) =>
          s"""```
             |Adds a match to the database
             |Usage: $symbolStr$AddMatchString <top_1_mention> ... <top_2_mention> <winning_team>
             |```""".stripMargin
        case Some(RelocateRoomsString) =>
          s"""```
             |Move match players to the right voice rooms
             |Usage: $symbolStr$RelocateRoomsString
             |```""".stripMargin
        case Some(RelocateLobbyString) =>
          s"""```
             |Move match players to the lobby voice room
             |Usage: $symbolStr$RelocateLobbyString
             |```""".stripMargin
        case _ =>
          val clear = s"$symbolStr$ClearString".pad(tablePadding)
          val addPlayer = s"$symbolStr$AddPlayerString".pad(tablePadding)
          val remove = s"$symbolStr$RemoveString".pad(tablePadding)
          val start = s"$symbolStr$StartString".pad(tablePadding)
          val abort = s"$symbolStr$AbortString".pad(tablePadding)
          val enrol = s"$symbolStr$EnrolString".pad(tablePadding)
          val winner = s"$symbolStr$WinnerString".pad(tablePadding)
          val reset = s"$symbolStr$ResetString".pad(tablePadding)
          val reprocess = s"$symbolStr$ReprocessString".pad(tablePadding)
          val addMatch = s"$symbolStr$AddMatchString".pad(tablePadding)
          val relocateRooms = s"$symbolStr$RelocateRoomsString".pad(tablePadding)
          val relocateLobby = s"$symbolStr$RelocateLobbyString".pad(tablePadding)
          val shutdown = s"$symbolStr$ShutdownString".pad(tablePadding)
          Seq(
            s"""$clear Clear the queue""",
            s"""$addPlayer Add the target player to the queue""",
            s"""$remove Remove the target player from the queue""",
            s"""$start Starts a match with the players in the queue""",
            s"""$abort Abort an ongoing match""",
            s"""$enrol Register another user""",
            s"""$winner Declare a winner for an on-going match""",
            s"""$reset Reset all players rating to default values""",
            s"""$reprocess Reprocess the matches in the database""",
            s"""$addMatch Adds a match to the database""",
            s"""$relocateRooms Move match players to the right voice rooms""",
            s"""$relocateLobby Move match players to the lobby voice room""",
            s"""$shutdown Shuts the bot down"""
          ).mkString("```Admin command list:\n\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(clear, addPlayer, remove, start, abort, enrol, shutdown, help, winner, reset, reprocess,
    addMatch, relocateRooms, relocateLobby)
}
