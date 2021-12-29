package com.scustoms.bot

import ackcord.commands.{CommandController, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.requests.{AddGuildMemberRole, CreateReaction}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, OptFuture}
import akka.NotUsed
import Emojis.{negativeMark, positiveMark}
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.StaticReferences
import com.scustoms.services.MatchService.ResolvedMatch
import com.scustoms.services.QueueService._
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.typesafe.config.Config

import scala.concurrent.Future

class UserCommands(config: Config,
                   queueService: QueueService,
                   playerService: PlayerService,
                   matchService: MatchService
                  )(implicit client: DiscordClient) extends CommandController(client.requests) {

  import com.scustoms.Utils.StringImprovements
  import com.scustoms.Utils.SeqImprovements

  val userCommandSymbols = Seq(config.getString("scustoms.userCommandSymbol"))
  val tablePadding: Int = config.getInt("scustoms.tablePadding")
  val shortTablePadding: Int = config.getInt("scustoms.shortTablePadding")

  val RegisteredString = "registered"
  val registered: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
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
              val header = Seq("Role", "# Games", "Win rate", "Rating").padConcat(shortTablePadding)
              val top = Seq("Top", player.topStats.games.toString, player.topStats.winRatePercentage, player.topStats.formattedRating).padConcat(shortTablePadding)
              val jungle = Seq("Jungle", player.jungleStats.games.toString, player.jungleStats.winRatePercentage, player.jungleStats.formattedRating).padConcat(shortTablePadding)
              val mid = Seq("Mid", player.midStats.games.toString, player.midStats.winRatePercentage, player.midStats.formattedRating).padConcat(shortTablePadding)
              val bot = Seq("Bot", player.botStats.games.toString, player.botStats.winRatePercentage, player.botStats.formattedRating).padConcat(shortTablePadding)
              val support = Seq("Support", player.supportStats.games.toString, player.supportStats.winRatePercentage, player.supportStats.formattedRating).padConcat(shortTablePadding)
              val message =
                s"""```
                   |In-game name: ${player.gameUsername}\n
                   |$header\n$top\n$jungle\n$mid\n$bot\n$support
                   |```""".stripMargin
              DiscordUtils.reactAndRespond(positiveMark, message)
            case None =>
              DiscordUtils.reactAndRespond(negativeMark, "Player not found. Make sure you have registered first.")
          }
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Failed to parse parameter as a player mention.")
      }
    })

  val LeaderboardString = "leaderboard"
  val leaderboard: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(LeaderboardString, "lederborde", "lederboard", "leatherboard"))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(playerService.getAllPlayers).flatMap {
        players =>
          val playerStrings = players
            .filter(_.totalGames > 0)
            .flatMap(p =>
              p.getBestStatistics.map(bestStats => (bestStats._2.rating.getConservativeRating, bestStats._1, bestStats._2, p))
            )
            .sortBy(_._1)
            .reverse
            .zipWithIndex
            .map { case ((rating, role, statistics, player), index) =>
              val paddedIndex = (index + 1).toString.pad(shortTablePadding)
              val paddedUsername = player.gameUsername.pad(tablePadding)
              val paddedRole = role.toString.pad(shortTablePadding)
              val paddedWinRate = statistics.winRatePercentage.appended('%').pad(shortTablePadding)
              val paddedGamesPlayed = statistics.games.toString.pad(shortTablePadding)
              val paddedRating = f"$rating%1.2f".pad(shortTablePadding)
              f"$paddedIndex$paddedUsername$paddedRole$paddedGamesPlayed$paddedWinRate$paddedRating"
            }
          val header = Seq(
            "#".pad(shortTablePadding),
            "Username".pad(tablePadding),
            "Role".pad(shortTablePadding),
            "# Games".pad(shortTablePadding),
            "Win rate".pad(shortTablePadding),
            "Rating".pad(shortTablePadding)
          ).reduceLeft(_ + _)
          val message = playerStrings.mkString(s"```$header\n\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    )

  val HistoryString = "history"
  val history: NamedComplexCommand[Option[Int], NotUsed] = GuildCommand
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
              val winner = if (teamAWon) "Team A".pad(shortTablePadding) else "Team B".pad(shortTablePadding)
              f"$winner$teamAString|$teamBString"
          }
          val header = s"${"Winner".pad(shortTablePadding)}${"Team 1".pad(shortTablePadding * 5)} ${"Team 2".pad(shortTablePadding * 5)}"
          val message = matchStrings.mkString(s"```$header\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    })

  val RegisterString = "register"
  val register: NamedComplexCommand[String, NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(RegisterString))
    .parsing[String]
    .asyncOpt(implicit m => {
      OptFuture.fromFuture(playerService.insert(m.user.id, m.user.username, m.parsed)).map {
        case Right(_) =>
          val changeRole = AddGuildMemberRole(m.guild.id, m.user.id, StaticReferences.customsRoleId)
          val react = CreateReaction(m.textChannel.id, m.message.id, positiveMark)
          val respond = m.textChannel.sendMessage(s"Player '${m.user.username}' successfully added")
          client.requestsHelper.runMany(react, respond, changeRole).map(_ => ())
        case Left(PlayerKeeper.PlayerAlreadyExists) =>
          DiscordUtils.reactAndRespond(negativeMark, s"Player '${m.user.username}' already exists")
        case Left(_) =>
          DiscordUtils.reactAndRespond(negativeMark, s"An unknown error has occurred")
      }
    })

  val JoinString = "join"
  val join: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(JoinString))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit command => {
      val result: Future[Either[QueueError, QueuedPlayer]] = if (queueService.contains(command.user.id)) {
        Future.successful(Left(PlayerAlreadyInQueue))
      } else if (matchService.contains(command.user.id)) {
        Future.successful(Left(PlayerAlreadyInMatch))
      } else {
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
      }

      OptFuture.fromFuture(result).map {
        case Right(queuedPlayer) =>
          queueService.addPlayer(queuedPlayer)
          val message = s"${command.user.mention} joined the queue (role: ${queuedPlayer.role}). Current queue size: ${queueService.length}"
          DiscordUtils.reactAndRespond(positiveMark, message)
        case Left(error) =>
          DiscordUtils.reactAndRespond(negativeMark, error.message)
      }
    })

  val LeaveString = "leave"
  val leave: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(LeaveString))
    .asyncOpt(implicit command =>
      if (queueService.remove(command.user.id))
        DiscordUtils.reactAndRespond(positiveMark, s"${command.user.mention} left the game or watchers queue. Current queue size: ${queueService.length}")
      else
        DiscordUtils.reactAndRespond(negativeMark, "You are not in the queue")
    )

  val WatchString = "watch"
  val watch: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(WatchString))
    .asyncOpt(implicit command => {
      if (queueService.contains(command.user.id)) {
        DiscordUtils.reactAndRespond(negativeMark, "You are already in the queue")
      } else if (matchService.contains(command.user.id)) {
        DiscordUtils.reactAndRespond(negativeMark, "You are already in a match")
      } else {
        queueService.addWatcher(command.user.id)
        val message = s"${command.user.mention} joined the watchers. You will now be pulled into voice rooms on games."
        DiscordUtils.reactAndRespond(positiveMark, message)
      }
    })

  val ShowString = "show"
  val show: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(ShowString))
    .withRequest(implicit m => {
      val allPlayersStrings = queueService.getQueue.zipWithIndex.map {
        case (QueuedPlayer(QueueService.Fill, player), index) =>
          s"${(index + 1).toString.pad(shortTablePadding)}${player.gameUsername.pad(tablePadding)}${QueueService.Fill.toString.pad(shortTablePadding)}"
        case (QueuedPlayer(role, player), index) =>
          val ratingStr = player.niceString(role.toMatchRole).pad(shortTablePadding)
          s"${(index + 1).toString.pad(shortTablePadding)}${player.gameUsername.pad(tablePadding)}${role.toString.pad(shortTablePadding)}$ratingStr"
      }
      val queueSize = s"Queue (${allPlayersStrings.length})".pad(shortTablePadding)
      val watchersSize = s"Watchers (${queueService.getWatchers.length})".pad(tablePadding)
      val header = s"${"#".pad(shortTablePadding)}${"Username".pad(tablePadding)}${"Role".pad(shortTablePadding)}${"Rating".pad(shortTablePadding)}"
      val playersString = allPlayersStrings.mkString(s"```$queueSize$watchersSize\n$header\n\n", "\n", "```")
      m.textChannel.sendMessage(playersString)
    })

  val HelpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq(HelpString))
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
        case Some(WatchString) =>
          s"""```
             |Get moved into the voice rooms on games
             |Usage: $symbolStr$WatchString
             |```""".stripMargin
        case Some(LeaveString) =>
          s"""```
             |Leave the game or watch queue
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
             |Usage: $symbolStr$LeaderboardString
             |```""".stripMargin
        case _ =>
          val registered = s"$symbolStr$RegisteredString".pad(tablePadding)
          val history = s"$symbolStr$HistoryString".pad(tablePadding)
          val register = s"$symbolStr$RegisterString".pad(tablePadding)
          val info = s"$symbolStr$InfoString".pad(tablePadding)
          val join = s"$symbolStr$JoinString".pad(tablePadding)
          val watch = s"$symbolStr$WatchString".pad(tablePadding)
          val leave = s"$symbolStr$LeaveString".pad(tablePadding)
          val show = s"$symbolStr$ShowString".pad(tablePadding)
          val leaderboard = s"$symbolStr$LeaderboardString".pad(tablePadding)
          Seq(
            s"""$registered Check if a user is registered""",
            s"""$history Look into the N latest matches""",
            s"""$register Register yourself in the database""",
            s"""$info Shows player information""",
            s"""$join Join the game queue""",
            s"""$watch Get moved into the voice rooms on games""",
            s"""$leave Leave the game or watch queue""",
            s"""$show Show current queue state""",
            s"""$leaderboard Show the leaderboard"""
          ).mkString("```User command list:\n\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(registered, history, watch, register, info, join, leave, show, leaderboard, help)
}
