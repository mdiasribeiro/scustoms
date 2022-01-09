package com.scustoms.bot

import ackcord.commands.{CommandController, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.requests.{AddGuildMemberRole, CreateReaction}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, OptFuture}
import akka.NotUsed
import Emojis.{negativeMark, positiveMark}
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.StaticReferences
import com.scustoms.database.keepers.PlayerStatisticsKeeper.PlayerStatistics
import com.scustoms.services.MatchService.{MatchRole, ResolvedMatch}
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.QueueService._
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.scustoms.trueskill.RatingUtils.ratingFormat
import com.typesafe.config.Config

import scala.concurrent.Future

class UserCommands(config: Config,
                   queueService: QueueService,
                   playerService: PlayerService,
                   matchService: MatchService
                  )(implicit client: DiscordClient) extends CommandController(client.requests) {

  import com.scustoms.Utils.StringImprovements
  import com.scustoms.Utils.SeqImprovements

  val userCommandsSymbol: String = config.getString("scustoms.userCommandSymbol")
  val userCommandSymbols = Seq(
    userCommandsSymbol,
    config.getString("scustoms.managerCommandSymbol"),
    config.getString("scustoms.adminCommandSymbol")
  )

  val tablePadding: Int = config.getInt("scustoms.tablePadding")
  val shortTablePadding: Int = config.getInt("scustoms.shortTablePadding")
  val indexPadding: Int = config.getInt("scustoms.indexPadding")

  val RegisteredString = "registered"
  val registered: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(RegisteredString))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit m => {
      val playerIdOpt = m.parsed
        .map(DiscordUtils.getUserIdFromMention)
        .getOrElse(Some(m.user.id))
      playerIdOpt match {
        case Some(playerId) =>
          OptFuture.fromFuture(playerService.exists(playerId)).flatMap {
            case true =>
              DiscordUtils.reactAndRespond(positiveMark, s"Player is already registered.")
            case false =>
              DiscordUtils.reactAndRespond(negativeMark, "Player not found.")
          }
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Failed to parse parameter as a player mention.")
      }
    })

  val InfoString = "info"
  val info: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(InfoString))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit userCommandMessage => {
      val playerIdOpt = userCommandMessage.parsed
        .map(DiscordUtils.getUserIdFromMention)
        .getOrElse(Some(userCommandMessage.user.id))
      playerIdOpt match {
        case Some(playerId) =>
          OptFuture.fromFuture(playerService.find(playerId)).flatMap {
            case Some(player) =>
              val message = DiscordUtils.playerToString(player, shortTablePadding)
              DiscordUtils.reactAndRespond(positiveMark, message)
            case None =>
              DiscordUtils.reactAndRespond(negativeMark, "Player not found. Make sure you have registered first.")
          }
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Failed to parse parameter as a player mention.")
      }
    })

  final val LeaderboardString = "leaderboard"
  final val LeaderboardShortString = "lb"
  case class FilteredStatistics(role: MatchRole, stats: PlayerStatistics, player: PlayerWithStatistics)

  def leaderboardRow(filteredStatistics: FilteredStatistics, index: Int)(additionalFields: FilteredStatistics => String): String = {
    val paddedIndex = (index + 1).toString.pad(indexPadding)
    val paddedUsername = filteredStatistics.player.gameUsername.pad(tablePadding)
    val paddedRole = filteredStatistics.role.toString.pad(shortTablePadding)
    val paddedWinRate = filteredStatistics.stats.winRatePercentage.appended('%').pad(shortTablePadding)
    val paddedGamesPlayed = filteredStatistics.stats.games.toString.pad(shortTablePadding)
    val otherColumns = additionalFields(filteredStatistics)
    f"$paddedIndex$paddedUsername$paddedRole$paddedGamesPlayed$paddedWinRate$otherColumns"
  }

  def leaderboardHeader(additionalHeaders: String): String = {
    s"${"#".pad(indexPadding)}${"Username".pad(tablePadding)}${"Role".pad(shortTablePadding)}${"# Games".pad(shortTablePadding)}${"Win rate".pad(shortTablePadding)}$additionalHeaders"
  }

  val leaderboard: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(LeaderboardString, LeaderboardShortString, "lederborde", "lederboard", "leatherboard", "ladderbored"))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit userCommandMessage => {
      val roleFilterOpt = userCommandMessage.parsed.flatMap(r => QueueService.parseRole(r).flatMap(_.toMatchRole))
      val boardTypeBest = userCommandMessage.parsed.exists(_.toLowerCase == "best")
      val minGames = 5
      OptFuture.fromFuture(playerService.getAllPlayers).flatMap {
        players =>
          val playerStrings = players
            .flatMap(p => roleFilterOpt match {
              case Some(role) =>
                val stats = p.getRoleStatistics(role)
                Option.when(stats.games > minGames)(FilteredStatistics(role, stats, p))
              case None =>
                p.getStatisticsBy(minGames) {
                  s => if (boardTypeBest) s.rating.getMean else s.games.toDouble
                }.map { case (role, stats) => FilteredStatistics(role, stats, p) }
            })
            .sortBy(_.stats.rating.getConservativeRating)
            .reverse
            .zipWithIndex
            .map { case (filteredStatistics, index) =>
              leaderboardRow(filteredStatistics, index) {
                fStats =>
                  val paddedMRating = ratingFormat(fStats.stats.rating.getMean).pad(shortTablePadding)
                  val paddedCRating = ratingFormat(fStats.stats.rating.getConservativeRating).pad(shortTablePadding)
                  s"$paddedMRating$paddedCRating"
              }
            }
          val header = leaderboardHeader(s"${"M. Rating".pad(shortTablePadding)}${"C. Rating".pad(shortTablePadding)}")
          val message = playerStrings.mkString(s"```$header\n\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    })

  val HistoryString = "history"
  val history: NamedComplexCommand[Option[Int], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(HistoryString))
    .parsing[Option[Int]](MessageParser.optional)
    .asyncOpt(implicit userCommandMessage => {
      val historySize = userCommandMessage.parsed.map(c => math.min(100, math.max(c, 0))).getOrElse(10)
      OptFuture.fromFuture(matchService.getLastN(historySize)).flatMap {
        matches =>
          val matchStrings = matches.map {
            case ResolvedMatch(teamAWon, teamA, teamB) =>
              val teamAString = teamA.seq.map(_.state.gameUsername.pad(shortTablePadding)).mkString("")
              val teamBString = teamB.seq.map(_.state.gameUsername.pad(shortTablePadding)).mkString("")
              val winnerA = if (teamAWon) "Won".pad(shortTablePadding) else "Lost".pad(shortTablePadding)
              val winnerB = if (!teamAWon) "Won".pad(shortTablePadding) else "Lost".pad(shortTablePadding)
              f"$winnerA$teamAString\n$winnerB$teamBString\n"
          }
          val header = s"${"Result".pad(shortTablePadding)}${"Teams".pad(shortTablePadding * 5)}"
          val message = matchStrings.mkString(s"```$header\n\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    })

  val RegisterString = "register"
  val register: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(RegisterString))
    .parsing[String]
    .asyncOpt(implicit m => {
      OptFuture.fromFuture(playerService.insert(m.user.id, m.user.username, m.parsed)).map {
        case Right(_) =>
          val changeRole = AddGuildMemberRole(m.guild.id, m.user.id, StaticReferences.customsRoleId)
          val react = CreateReaction(m.textChannel.id, m.message.id, positiveMark)
          val respond = m.textChannel.sendMessage(s"${m.user.mention} you have been successfully registered. Say `$$help` to learn about actions you can take.")
          client.requestsHelper.runMany(react, respond, changeRole).map(_ => ())
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          DiscordUtils.reactAndRespond(negativeMark, s"Player '${m.user.username}' already exists")
        case Left(_) =>
          DiscordUtils.reactAndRespond(negativeMark, s"An unknown error has occurred")
      }
    })

  val JoinString = "join"
  val JoinShortString = "j"
  val join: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(JoinString, JoinShortString))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit command => {
      val result: Future[Either[QueueError, QueuedPlayer]] =
        playerService.find(command.user.id).map {
          case Some(player) =>
            command.parsed
              .map(QueueService.parseRole)
              .getOrElse(Some(QueueService.Fill))
              .toRight(ErrorParsingRole)
              .map(role => QueuedPlayer(role, player))
          case None =>
            Left(PlayerDoesNotExist)
        }

      OptFuture.fromFuture(result).map {
        case Right(queuedPlayer) =>
          val isInPriorityQueue = queueService.containsPriorityPlayer(queuedPlayer.discordId)
          val isInNormalQueue = queueService.containsNormalPlayer(queuedPlayer.discordId)
          val message = if (isInPriorityQueue) {
            queueService.upsertPriorityPlayer(queuedPlayer)
            s"${command.user.mention} changed queued role to ${queuedPlayer.role}."
          } else if (isInNormalQueue) {
            queueService.upsertNormalPlayer(queuedPlayer)
            s"${command.user.mention} changed queued role to ${queuedPlayer.role}."
          } else {
            queueService.upsertNormalPlayer(queuedPlayer)
            s"${command.user.mention} joined the queue (${queuedPlayer.role}). Queue size: ${queueService.queueSize}"
          }
          DiscordUtils.respond(message)
        case Left(error) =>
          DiscordUtils.reactAndRespond(negativeMark, error.message)
      }
    })

  val LeaveString = "leave"
  val leave: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(LeaveString))
    .asyncOpt(implicit command =>
      if (queueService.removeNormalPlayer(command.user.id))
        DiscordUtils.reactAndRespond(positiveMark, s"${command.user.mention} left the queue. Queue size: ${queueService.queueSize}")
      else if (queueService.removePriorityPlayer(command.user.id))
        DiscordUtils.reactAndRespond(positiveMark, s"${command.user.mention} left the priority queue. Queue size: ${queueService.queueSize}")
      else
        DiscordUtils.reactAndRespond(negativeMark, "You are not watching or in the queue")
    )

  val ShowString = "show"
  def queuedPlayersToString(players: Seq[QueuedPlayer]): Seq[String] = {
    players.sortBy(_.role)(QueueService.RoleOrdering).map {
      case QueuedPlayer(QueueService.Fill, player) =>
        s"${player.gameUsername.pad(tablePadding)}${QueueService.Fill.toString.pad(shortTablePadding)}"
      case QueuedPlayer(role, player) =>
        val ratingStr = role.toMatchRole.map(player.conservativeRatingToString).getOrElse("").pad(shortTablePadding)
        s"${player.gameUsername.pad(tablePadding)}${role.toString.pad(shortTablePadding)}$ratingStr"
    }
  }
  val show: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(ShowString))
    .withRequest(implicit m => {
      val prioPlayers = queuedPlayersToString(queueService.getPriorityQueue)
      val normalPlayers = queuedPlayersToString(queueService.getNormalQueue)
      val header = s"${"Username".pad(tablePadding)}${"Role".pad(shortTablePadding)}${"Rating".pad(shortTablePadding)}"
      val prioQueueBlock = prioPlayers.ifNonEmpty(
        s"""${s"Prio queue (${prioPlayers.length})".pad(tablePadding)}
        |$header
        |${prioPlayers.mkString("\n")}
        |\n""".stripMargin
      )
      val queueBlock = normalPlayers.ifNonEmpty(
        s"""${s"Queue (${normalPlayers.length})".pad(tablePadding)}
        |$header
        |${normalPlayers.mkString("\n")}
        |\n""".stripMargin
      )
      val message = if (prioPlayers.isEmpty && normalPlayers.isEmpty)
        "The queue is currently empty"
      else
        DiscordUtils.codeBlock(s"$prioQueueBlock$queueBlock")
      m.textChannel.sendMessage(message)
    })

  val HelpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(Seq(userCommandsSymbol), Seq(HelpString))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(implicit m => {
      val symbolStr = userCommandSymbols.head
      val helpText = m.parsed match {
        case Some(RegisteredString) =>
          s"""```
             |Check if a user is registered
             |Usage: $symbolStr$RegisteredString <?mention>
             |```""".stripMargin
        case Some(HistoryString) =>
          s"""```
             |Look into the N latest matches.
             |Usage: $symbolStr$HistoryString <?N>
             |N will be capped within the range [0, 100]
             |```""".stripMargin
        case Some(RegisterString) =>
          s"""```
             |Register yourself in the database (required to participate)
             |Usage: $symbolStr$RegisterString <username>
             |```""".stripMargin
        case Some(InfoString) =>
          s"""```
             |Shows player information
             |Usage: $symbolStr$InfoString <?mention>
             |```""".stripMargin
        case Some(JoinString) =>
          s"""```
             |Join the game queue
             |Usage: $symbolStr$JoinString <?role>
             |(Possible roles: top, jungle, mid, bot, sup/support)
             |```""".stripMargin
        case Some(LeaveString) =>
          s"""```
             |Leave the queue
             |Usage: $symbolStr$LeaveString
             |```""".stripMargin
        case Some(ShowString) =>
          s"""```
             |Show current queue state
             |Usage: $symbolStr$ShowString
             |```""".stripMargin
        case Some(LeaderboardString) =>
          s"""```
             |Show the leaderboard
             |Usage: $symbolStr$LeaderboardString <board_type>
             |(Possible types: top, jungle, mid, bot, sup, support, best)
             |```""".stripMargin
        case _ =>
          val registered = s"$symbolStr$RegisteredString".pad(tablePadding)
          val history = s"$symbolStr$HistoryString".pad(tablePadding)
          val register = s"$symbolStr$RegisterString".pad(tablePadding)
          val info = s"$symbolStr$InfoString".pad(tablePadding)
          val join = s"$symbolStr$JoinString".pad(tablePadding)
          val leave = s"$symbolStr$LeaveString".pad(tablePadding)
          val show = s"$symbolStr$ShowString".pad(tablePadding)
          val leaderboard = s"$symbolStr$LeaderboardString".pad(tablePadding)
          Seq(
            s"""$registered Check if a user is registered""",
            s"""$history Look into the N latest matches""",
            s"""$register Register yourself in the database""",
            s"""$info Shows player information""",
            s"""$join Join the game queue""",
            s"""$leave Leave the queue""",
            s"""$show Show current queue state""",
            s"""$leaderboard Show the leaderboard"""
          ).mkString("```User command list:\n\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(registered, history, register, info, join, leave, show, leaderboard, help)
}
