package com.scustoms.bot

import ackcord.commands._
import ackcord.data.{RoleId, UserId}
import ackcord.requests.{AddGuildMemberRole, CreateReaction, ModifyGuildMember, ModifyGuildMemberData}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, JsonSome, OptFuture}
import akka.NotUsed
import com.scustoms.Utils
import com.scustoms.bot.Emojis.{negativeMark, positiveMark}
import com.scustoms.database.{DatabaseManager, StaticReferences}
import com.scustoms.database.keepers.MatchKeeper.{StoredMatch, StoredMatchTeam}
import com.scustoms.services.MatchService.{MatchPlayer, OngoingMatch}
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.QueueService.QueuedPlayer
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
  final val ClearShortString = "clear"
  val clear: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(ClearString, ClearShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit m => {
      m.parsed match {
        case Some("all") =>
          queueService.clearAll()
          DiscordUtils.reactAndRespond(positiveMark, "All queues have been cleared")
        case Some("prio") =>
          queueService.clearPriorityQueue()
          DiscordUtils.reactAndRespond(positiveMark, "Priority queue has been cleared")
        case _ =>
          queueService.clearNormalQueue()
          DiscordUtils.reactAndRespond(positiveMark, "Normal queue has been cleared")
      }
    })

  def addPlayerToQueue[T](queueLambda: QueuedPlayer => Boolean, mention: String, role: Option[String])
                         (implicit command: GuildMemberCommandMessage[T]): OptFuture[Unit] = {
    val parsedUserId = DiscordUtils.getUserIdFromMention(mention)
    val parsedRole = role.map(QueueService.parseRole).getOrElse(Some(QueueService.Fill))
    (parsedUserId, parsedRole) match {
      case (Right(userId), Some(role)) =>
        OptFuture.fromFuture(playerService.find(userId)).map {
          case Right(player) =>
            queueLambda(QueueService.QueuedPlayer(role, player))
            DiscordUtils.reactAndRespond(positiveMark, s"$mention was added to the queue with role: $role")
          case Left(_) =>
            DiscordUtils.reactAndRespond(negativeMark, s"$mention could not be found. Make sure he is registered.")
        }
      case (Right(_), None) =>
        DiscordUtils.reactAndRespond(negativeMark, s"Player role `$role` could not be parsed")
      case (Left(err), _) =>
        DiscordUtils.reactAndRespond(negativeMark, err.message)
    }
  }

  final val AddPlayerString = "addPlayer"
  final val AddPlayerShortString = "add"
  val addPlayer: NamedComplexCommand[(String, Option[String]), NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AddPlayerString, AddPlayerShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, Option[String])](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit command => {
      def addToQueue(p: QueuedPlayer): Boolean = queueService.upsertNormalPlayer(p)
      addPlayerToQueue(addToQueue, command.parsed._1, command.parsed._2)
    })

  final val AddPriorityPlayerString = "addPrioPlayer"
  final val AddPriorityPlayerShortString = "addPrio"
  val addPriorityPlayer: NamedComplexCommand[(String, Option[String]), NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AddPriorityPlayerString, AddPriorityPlayerShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, Option[String])](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit command => {
      def addToQueue(p: QueuedPlayer): Boolean = queueService.upsertPriorityPlayer(p)
      addPlayerToQueue(addToQueue, command.parsed._1, command.parsed._2)
    })

  final val SwapPlayersString = "swapPlayers"
  final val SwapPlayersShortString = "swap"
  val swapPlayers: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(SwapPlayersString, SwapPlayersShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, String)](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit command => {
      val parsedUserId1 = DiscordUtils.getUserIdFromMention(command.parsed._1)
      val parsedUserId2 = DiscordUtils.getUserIdFromMention(command.parsed._2)
      val ongoingMatch = matchService.ongoingMatch
      (parsedUserId1, parsedUserId2, ongoingMatch) match {
        case (Right(userId1), Right(userId2), Some(ongoingMatch)) =>
          val result = (ongoingMatch.contains(userId1), ongoingMatch.contains(userId2)) match {
            case (true, true) =>
              Future.successful(matchService.swapPlayers(userId1, userId2))
            case (true, false) =>
              playerService.findAndResolve(userId2).map(_.toOption.flatMap(player2Data => matchService.swapPlayer(userId1, player2Data)))
            case (false, true) =>
              playerService.findAndResolve(userId1).map(_.toOption.flatMap(player1Data => matchService.swapPlayer(userId2, player1Data)))
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
  final val RemoveShortString = "remove"
  val remove: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RemoveString, RemoveShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[String]
    .asyncOpt(implicit command => {
      DiscordUtils.getUserIdFromMention(command.parsed) match {
        case Right(userId) if queueService.containsNormalPlayer(userId) =>
          queueService.remove(userId)
          DiscordUtils.respond(s"${command.parsed} has been removed from the queue")
        case Right(userId) if queueService.containsPriorityPlayer(userId) =>
          queueService.remove(userId)
          DiscordUtils.respond(s"${command.parsed} has been removed from the priority queue")
        case Right(_) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player was not found in the queue")
        case Left(err) =>
          DiscordUtils.reactAndRespond(negativeMark, err.message)
      }
    })

  final val StartString = "startMatch"
  final val StartShortString = "start"
  def partitionResolvePlayers(players: Seq[QueuedPlayer]): Future[Either[DatabaseManager.DatabaseError, (Seq[MatchPlayer], Seq[PlayerWithStatistics])]] = {
    val resolvedPlayers = players.map(queuedPlayer =>
      playerService.resolvePlayer(queuedPlayer.player)
        .map(_.map(resolvedPlayer => (queuedPlayer.role.toMatchRole, resolvedPlayer)))
    )
    for {
      resolvedPlayersFuture <- Future.sequence(resolvedPlayers)
      resolvedP = Utils.sequenceEither(resolvedPlayersFuture)
    } yield resolvedP.map(_.partitionMap {
      case (Some(role), p) => Left(MatchPlayer(role, p))
      case (None, p) => Right(p)
    })
  }
  val start: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(StartString, StartShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(DiscordUtils.withErrorHandler(_) {
      implicit m => {
        val prioPlayers = queueService.getPriorityQueue
        val players = if (prioPlayers.isEmpty) {
          queueService.getNormalQueue
        } else {
          prioPlayers ++ queueService.getRandomN(10 - prioPlayers.length)
        }
        OptFuture.fromFuture(partitionResolvePlayers(players)).map {
          case Left(err) =>
            DiscordUtils.reactAndRespond(negativeMark, err.message)
          case Right((rolePlayers, fillPlayers)) =>
            val startingMatch = RatingUtils.calculateRoleMatch(rolePlayers, fillPlayers)
            matchService.ongoingMatch = Some(startingMatch)
            val remainingPlayers = queueService.remaining(startingMatch)
            val msg = DiscordUtils.ongoingMatchToString(startingMatch, remainingPlayers, tablePadding)
            client.requestsHelper.run(m.textChannel.sendMessage(DiscordUtils.codeBlock(msg))).map(_ => ())
        }
      }
    })

  final val RebalanceString = "rebalanceMatch"
  final val RebalanceShortString = "rebalance"
  val rebalance: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RebalanceString, RebalanceShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(DiscordUtils.withErrorHandler(_) {
      implicit m => {
        matchService.ongoingMatch match {
          case Some(ongoingMatch) =>
            val startingMatch = RatingUtils.reBalanceMatch(ongoingMatch)
            matchService.ongoingMatch = Some(startingMatch)
            val msg = DiscordUtils.ongoingMatchToString(startingMatch, Seq.empty, tablePadding)
            DiscordUtils.respond(msg)
          case None =>
            DiscordUtils.reactAndRespond(negativeMark, s"There is no on-going match to re-balance.")
        }
      }
    })

  final val AbortString = "abortMatch"
  final val AbortShortString = "abort"
  val abort: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AbortString, AbortShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => {
      matchService.ongoingMatch match {
        case Some(_) =>
          matchService.ongoingMatch = None
          queueService.clearAll()
          val mention = StaticReferences.customsRoleId.resolve.map(_.mention).getOrElse("Players")
          DiscordUtils.reactAndRespond(positiveMark, s"Match was aborted. $mention, join the queue again.")
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no on-going match to abort.")
      }
    })

  final val EnrolString = "enrolPlayer"
  final val EnrolShortString = "enrol"
  val enrol: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(EnrolString, EnrolShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, String)](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit m => {
      val result: Either[String, Future[Either[String, (String, UserId)]]] = for {
        targetId <- DiscordUtils.getUserIdFromMention(m.parsed._1).left.map(_.message)
        targetUser <- targetId.resolve.toRight(DiscordUtils.PlayerNotInServer(m.parsed._1).message)
      } yield playerService.insert(targetId, targetUser.username, m.parsed._2).map {
        case Right(_) =>
          Right((s"Player '${targetUser.username}' successfully added", targetId))
        case Left(err) =>
          Left(err.message)
      }

      OptFuture
        .fromFuture(Utils.foldEitherOfFuture(result).map(_.flatten))
        .flatMap {
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
  final val RenameShortString = "rename"
  val renamePlayer: NamedComplexCommand[(String, String), NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RenamePlayerString, RenameShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[(String, String)](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit m => {
      val result = DiscordUtils.getUserIdFromMention(m.parsed._1)
        .map(targetId => playerService.updateGameUsername(targetId, m.parsed._2))

      OptFuture.fromFuture(Utils.foldEitherOfFuture(result)).flatMap {
        case Left(err) =>
          DiscordUtils.reactAndRespond(negativeMark, err.message)
        case Right(Left(_)) =>
          DiscordUtils.reactAndRespond(negativeMark, "Player could not be found in the database. Make sure they are registered.")
        case Right(Right(_)) =>
          DiscordUtils.reactAndRespond(positiveMark, "Player has been successfully renamed.")
      }
    })

  final val WinnerString = "declareWinner"
  final val WinnerShortString = "winner"
  val winner: NamedComplexCommand[Int, NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(WinnerString, WinnerShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[Int]
    .asyncOpt(implicit m => {
      val winningTeam = DiscordUtils.parseWinningTeamA(m.parsed)
      (winningTeam, matchService.ongoingMatch) match {
        case (_, None) =>
          DiscordUtils.reactAndRespond(negativeMark, s"There is no ongoing match.")
        case (Left(err), _) =>
          DiscordUtils.reactAndRespond(negativeMark, err.message)
        case (Right(team1Won), Some(ongoingMatch)) =>
          matchService.ongoingMatch = None
          val playingPlayers = ongoingMatch.team1.seq ++ ongoingMatch.team2.seq
          val remainingPlayers = queueService.remaining(ongoingMatch)
          queueService.updatePriorities(playingPlayers, remainingPlayers)
          queueService.clearNormalQueue()
          val completeMatch = RatingUtils.calculate(ongoingMatch, team1Won)
          val updateResult = matchService.insertAndUpdate(completeMatch)
            .map(_ => "Game result has been saved and player ratings updated.")
            .recover(err => s"An error has occurred: ${err.getMessage}")
          OptFuture.fromFuture(updateResult).flatMap(DiscordUtils.respond(_))
      }
    })

  final val CorrectString = "correctLast"
  final val CorrectShortString = "correct"
  val correct: NamedComplexCommand[Int, NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(CorrectString, CorrectShortString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[Int]
    .asyncOpt(implicit m => {
      OptFuture.fromFuture(matchService.getLastN(1))
        .flatMap(lastMatch => (lastMatch, DiscordUtils.parseWinningTeamA(m.parsed)) match {
          case (Seq(lastMatch), Right(team1Won)) =>
            matchService.changeResult(lastMatch.id, team1Won)
            DiscordUtils.reactAndRespond(positiveMark, "Last match result has been changed.")
          case (_, Left(err)) =>
            DiscordUtils.reactAndRespond(negativeMark, err.message)
          case _ =>
            DiscordUtils.reactAndRespond(negativeMark, "Last match could not be retrieved.")
        })
    })

  final val AddMatchString = "addMatch"
  case class AddMatchParams(t1: String, j1: String, m1: String, b1: String, s1: String,
                            t2: String, j2: String, m2: String, b2: String, s2: String, winningTeam: Int)
  def resolveAddMatch(m: AddMatchParams): Either[DiscordUtils.DiscordError, StoredMatch] =
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
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(AddMatchString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[AddMatchParams](MessageParser.Auto.deriveParser)
    .asyncOpt(implicit m => {
      resolveAddMatch(m.parsed) match {
        case Right(params) =>
          OptFuture.fromFuture(matchService.resolveStoredMatch(params)).map {
            case Some(resolvedMatch) =>
              val ongoingMatch = OngoingMatch.fromResolvedStoredMatch(resolvedMatch)
              val completeMatch = RatingUtils.calculate(ongoingMatch, resolvedMatch.team1Won)
              matchService
                .insertAndUpdate(completeMatch)
                .map(_ => DiscordUtils.reactAndRespond(positiveMark, "Game result has been saved and player ratings updated."))
            case None =>
              DiscordUtils.reactAndRespond(negativeMark, "A player could not be resolved, make sure everyone is registered.")
          }
        case Left(err) =>
          DiscordUtils.reactAndRespond(negativeMark, err.message)
      }
    })

  final val RelocateRoomsString = "relocateRooms"
  val relocateRooms: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RelocateRoomsString, "recolaterooms", "rr"))
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
          val allPlayers = moveTeamA ++ moveTeamB
          client.requestsHelper.runMany(allPlayers: _*).map(_ => ())
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "There is no ongoing match")
      }
    })

  final val RelocateLobbyString = "relocateLobby"
  val relocateLobby: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq(RelocateLobbyString, "recolatelobby", "rl"))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => {
      matchService.ongoingMatch match {
        case Some(ongoingMatch) =>
          val allMembers = ongoingMatch.team1.seq.map(_.state.discordId) ++ ongoingMatch.team2.seq.map(_.state.discordId)
          val moveTeams = allMembers.map(playerId => {
            val newData = ModifyGuildMemberData(channelId = JsonSome(StaticReferences.lobbyChannel))
            ModifyGuildMember(m.guild.id, playerId, newData)
          })
          client.requestsHelper.runMany(moveTeams: _*).map(_ => ())
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "There is no ongoing match")
      }
    })

  val test: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(managerCommandSymbols, Seq("test"))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[String]
    .asyncOpt(implicit m => {
      DiscordUtils.respond(s"&${m.parsed}")
    })

  val helpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(Seq(managerCommandsSymbol), Seq(helpString))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(m => {
      val symbolStr = managerCommandSymbols.head
      val helpText = m.parsed match {
        case Some(ClearString) =>
          s"""```
             |Clear the queue
             |Usage: $symbolStr$ClearString <?parameter>
             |(Possible parameters: all, prio)
             |```""".stripMargin
        case Some(AddPlayerString) =>
          s"""```
             |Add the target player to the queue
             |Usage: $symbolStr$AddPlayerString <mention> <?role>
             |(Possible roles: top, jungle, mid, bot, sup/support, fill/any/empty)
             |```""".stripMargin
        case Some(AddPriorityPlayerString) =>
          s"""```
             |Add the target player to the priority queue
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
        case Some(RebalanceString) =>
          s"""```
             |Rebalances the ongoing match
             |Usage: $symbolStr$RebalanceString
             |```""".stripMargin
        case Some(WinnerString) =>
          s"""```
             |Declare a winner for an on-going match
             |Usage: $symbolStr$WinnerString <winning_team>
             |Winning team parameter should be either the integer 1 or 2
             |```""".stripMargin
        case Some(CorrectString) =>
          s"""```
             |Modifies the result of the last declared match
             |Usage: $symbolStr$CorrectString <winning_team>
             |Winning team parameter should be either the integer 1 or 2
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
          val addPriorityPlayer = s"$symbolStr$AddPriorityPlayerString".pad(tablePadding)
          val swapPlayers = s"$symbolStr$SwapPlayersString".pad(tablePadding)
          val remove = s"$symbolStr$RemoveString".pad(tablePadding)
          val start = s"$symbolStr$StartString".pad(tablePadding)
          val rebalance = s"$symbolStr$RebalanceString".pad(tablePadding)
          val winner = s"$symbolStr$WinnerString".pad(tablePadding)
          val correct = s"$symbolStr$CorrectString".pad(tablePadding)
          val abort = s"$symbolStr$AbortString".pad(tablePadding)
          val enrol = s"$symbolStr$EnrolString".pad(tablePadding)
          val rename = s"$symbolStr$RenamePlayerString".pad(tablePadding)
          val addMatch = s"$symbolStr$AddMatchString".pad(tablePadding)
          val relocateRooms = s"$symbolStr$RelocateRoomsString".pad(tablePadding)
          val relocateLobby = s"$symbolStr$RelocateLobbyString".pad(tablePadding)
          Seq(
            s"""$clear Clear the queue""",
            s"""$addPlayer Add the target player to the queue""",
            s"""$addPriorityPlayer Add the target player to the priority queue""",
            s"""$swapPlayers Swap one in-game player with another""",
            s"""$remove Remove the target player from the queue""",
            s"""$start Starts a match with the players in the queue""",
            s"""$rebalance Rebalances the ongoing match""",
            s"""$winner Declare a winner for an on-going match""",
            s"""$correct Modifies the result of the last declared match""",
            s"""$abort Abort an ongoing match""",
            s"""$enrol Register another user""",
            s"""$rename Change the game username of the target user""",
            s"""$addMatch Adds a match to the database""",
            s"""$relocateRooms Move match players to the teams voice rooms""",
            s"""$relocateLobby Move match players to the lobby voice room""",
          ).mkString("```Manager command list:\n\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(clear, addPlayer, addPriorityPlayer, remove, start, rebalance, winner, correct, abort, enrol,
    renamePlayer, help, addMatch, relocateRooms, relocateLobby, swapPlayers, test)
}
