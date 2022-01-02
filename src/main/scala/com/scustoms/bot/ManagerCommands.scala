package com.scustoms.bot

import ackcord.commands._
import ackcord.data.{RoleId, UserId}
import ackcord.requests.{AddGuildMemberRole, CreateReaction, ModifyGuildMember, ModifyGuildMemberData}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, JsonSome, OptFuture}
import akka.NotUsed
import com.scustoms.Utils
import com.scustoms.bot.Emojis.{negativeMark, positiveMark}
import com.scustoms.database.StaticReferences
import com.scustoms.database.keepers.MatchKeeper.{StoredMatch, StoredMatchTeam}
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.services.MatchService.OngoingMatch
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.scustoms.trueskill.RatingUtils
import com.typesafe.config.Config

import scala.concurrent.Future

class ManagerCommands(config: Config,
                      queueService: QueueService,
                      playerService: PlayerService,
                      matchService: MatchService
                   )(implicit client: DiscordClient) extends CommandController(client.requests) {

  import com.scustoms.Utils.StringImprovements

  val managerCommandsSymbol: String = config.getString("scustoms.managerCommandSymbol")
  val managerCommandSymbols = Seq(
    managerCommandsSymbol,
    config.getString("scustoms.adminCommandSymbol")
  )
  val tablePadding: Int = config.getInt("scustoms.tablePadding")
  val requiredRole: RoleId = StaticReferences.managerRoleId

  final val ClearString = "clearQueue"
  val clear: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(ClearString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => {
      queueService.clear()
      DiscordUtils.reactAndRespond(positiveMark, "Queue has been cleared")
    })

  final val AddPlayerString = "addPlayer"
  val addPlayer: NamedComplexCommand[(String, Option[String]), NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AddPlayerString))
    .andThen(DiscordUtils.needRole(requiredRole))
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
              queueService.upsertPlayer(QueueService.QueuedPlayer(role, player))
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

  final val SwapPlayersString = "swapPlayers"
  val swapPlayers: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(SwapPlayersString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, String)](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit command => {
      val parsedUserId1 = DiscordUtils.getUserIdFromMention(command.parsed._1)
      val parsedUserId2 = DiscordUtils.getUserIdFromMention(command.parsed._2)
      val ongoingMatch = matchService.ongoingMatch
      (parsedUserId1, parsedUserId2, ongoingMatch) match {
        case (Some(userId1), Some(userId2), Some(ongoingMatch)) =>
          val result = (ongoingMatch.contains(userId1), ongoingMatch.contains(userId2)) match {
            case (true, true) =>
              Future.successful(matchService.swapPlayers(userId1, userId2))
            case (true, false) =>
              playerService.find(userId2).map(_.flatMap(player2Data => matchService.swapPlayer(userId1, player2Data)))
            case (false, true) =>
              playerService.find(userId1).map(_.flatMap(player1Data => matchService.swapPlayer(userId2, player1Data)))
            case (false, false) =>
              Future.successful(None)
          }
          OptFuture.fromFuture(result).map {
            case Some(newMatch) =>
              matchService.ongoingMatch = Some(newMatch)
              val remainingPlayers = queueService.remaining(newMatch)
              val msg = DiscordUtils.ongoingMatchToString(newMatch, remainingPlayers, tablePadding)
              DiscordUtils.reactAndRespond(positiveMark, msg)
            case None =>
              DiscordUtils.reactAndRespond(negativeMark, s"At least one player could not be found")
          }
        case (_, _, None) =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing game")
        case (_, _, Some(_)) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player mention could not be parsed")
      }
    })

  final val RemoveString = "removePlayer"
  val remove: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RemoveString))
    .andThen(DiscordUtils.needRole(requiredRole))
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(StartString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => {
      try {
        val startingMatch = RatingUtils.calculateRoleMatch(queueService.getQueue)
        matchService.ongoingMatch = Some(startingMatch)
        val remainingPlayers = queueService.remaining(startingMatch)
        val msg = DiscordUtils.ongoingMatchToString(startingMatch, remainingPlayers, tablePadding)
        client.requestsHelper.run(m.textChannel.sendMessage(msg)).map(_ => ())
      } catch {
        case err: Exception =>
          DiscordUtils.reactAndRespond(negativeMark, s"Error: ${err.getMessage}")
      }
    })

  final val AbortString = "abortMatch"
  val abort: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AbortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => {
      matchService.ongoingMatch match {
        case Some(_) =>
          matchService.ongoingMatch = None
          queueService.clearPlayers()
          val mention = StaticReferences.customsRoleId.resolve.map(_.mention).getOrElse("Players")
          DiscordUtils.reactAndRespond(positiveMark, s"Match was aborted. $mention, join the queue again.")
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no on-going match to abort.")
      }
    })

  final val EnrolString = "enrolPlayer"
  val enrol: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(EnrolString))
    .andThen(DiscordUtils.needRole(requiredRole))
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

  final val RenamePlayerString = "renamePlayer"
  val renamePlayer: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RenamePlayerString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, String)](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit m => {
      val result = DiscordUtils.getUserIdFromMention(m.parsed._1)
        .toRight("Mention could not be parsed")
        .map(targetId => playerService.updateGameUsername(targetId, m.parsed._2))

      OptFuture.fromFuture(Utils.foldEitherOfFuture(result)).flatMap {
        case Left(err) =>
          DiscordUtils.reactAndRespond(negativeMark, err)
        case Right(Left(_)) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player could not be found in the database. Make sure they are registered.")
        case Right(Right(_)) =>
          DiscordUtils.reactAndRespond(positiveMark, "Player has been successfully renamed.")
      }
    })

  final val WinnerString = "declareWinner"
  val winner: NamedComplexCommand[Int, NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(WinnerString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[Int]
    .asyncOpt(implicit m => {
      val winningTeam = DiscordUtils.parseWinningTeamA(m.parsed)
      (winningTeam, matchService.ongoingMatch) match {
        case (_, None) =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing match.")
        case (None, _) =>
          DiscordUtils.reactAndRespond(negativeMark, "Team number is not valid. Must be 1 or 2.")
        case (Some(team1Won), Some(ongoingMatch)) =>
          queueService.clearPlayers()
          matchService.ongoingMatch = None
          val completeMatch = RatingUtils.calculate(ongoingMatch, team1Won)
          val update = matchService.insertAndUpdate(completeMatch)
          OptFuture.fromFuture(update)
            .map(_ => DiscordUtils.reactAndRespond(positiveMark, "Game result has been saved and player ratings updated."))
      }
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AddMatchString))
    .andThen(DiscordUtils.needRole(requiredRole))
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RelocateRoomsString))
    .andThen(DiscordUtils.needRole(requiredRole))
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RelocateLobbyString))
    .andThen(DiscordUtils.needRole(requiredRole))
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(Seq(managerCommandsSymbol), Seq(helpString))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(m => {
      val symbolStr = managerCommandSymbols.head
      val helpText = m.parsed match {
        case Some(ClearString) =>
          s"""```
             |Clear the queue
             |Usage: $symbolStr$ClearString
             |```""".stripMargin
        case Some(AddPlayerString) =>
          s"""```
             |Add the target player to the queue
             |Usage: $symbolStr$AddPlayerString <mention> <?role>
             |(Possible roles: top, jungle, mid, bot, sup/support, fill/any/empty)
             |```""".stripMargin
        case Some(SwapPlayersString) =>
          s"""```
             |Swap at least one player in a match with another player
             |Usage: $symbolStr$SwapPlayersString <mention1> <mention2>
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
             |Abort an ongoing match
             |Usage: $symbolStr$AbortString
             |```""".stripMargin
        case Some(EnrolString) =>
          s"""```
             |Register another user
             |Usage: $symbolStr$EnrolString <mention>
             |```""".stripMargin
        case Some(RenamePlayerString) =>
          s"""```
             |Change the game username of the mentioned player
             |Usage: $symbolStr$RenamePlayerString <mention>
             |```""".stripMargin
        case Some(WinnerString) =>
          s"""```
             |Declare a winner for an on-going match
             |Usage: $symbolStr$WinnerString <winning_team>
             |Winning team parameter should be either the integer 1 or 2
             |```""".stripMargin
        case Some(AddMatchString) =>
          s"""```
             |Adds a match to the database
             |Usage: $symbolStr$AddMatchString <top_1_mention> ... <support_2_mention> <winning_team>
             |```""".stripMargin
        case Some(RelocateRoomsString) =>
          s"""```
             |Move match players to the teams voice rooms
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
          val swapPlayers = s"$symbolStr$SwapPlayersString".pad(tablePadding)
          val remove = s"$symbolStr$RemoveString".pad(tablePadding)
          val start = s"$symbolStr$StartString".pad(tablePadding)
          val abort = s"$symbolStr$AbortString".pad(tablePadding)
          val enrol = s"$symbolStr$EnrolString".pad(tablePadding)
          val rename = s"$symbolStr$RenamePlayerString".pad(tablePadding)
          val winner = s"$symbolStr$WinnerString".pad(tablePadding)
          val addMatch = s"$symbolStr$AddMatchString".pad(tablePadding)
          val relocateRooms = s"$symbolStr$RelocateRoomsString".pad(tablePadding)
          val relocateLobby = s"$symbolStr$RelocateLobbyString".pad(tablePadding)
          Seq(
            s"""$clear Clear the queue""",
            s"""$addPlayer Add the target player to the queue""",
            s"""$swapPlayers Swap one in-game player with another""",
            s"""$remove Remove the target player from the queue""",
            s"""$start Starts a match with the players in the queue""",
            s"""$abort Abort an ongoing match""",
            s"""$enrol Register another user""",
            s"""$rename Change the game username of the target user""",
            s"""$winner Declare a winner for an on-going match""",
            s"""$addMatch Adds a match to the database""",
            s"""$relocateRooms Move match players to the teams voice rooms""",
            s"""$relocateLobby Move match players to the lobby voice room""",
          ).mkString("```Manager command list:\n\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(clear, addPlayer, remove, start, abort, enrol, renamePlayer, help, winner,
    addMatch, relocateRooms, relocateLobby, swapPlayers)
}
