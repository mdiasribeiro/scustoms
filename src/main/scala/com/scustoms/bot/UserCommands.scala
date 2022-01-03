package com.scustoms.bot

import ackcord.commands.{CommandController, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.requests.{AddGuildMemberRole, CreateReaction}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, OptFuture}
import akka.NotUsed
import Emojis.{negativeMark, positiveMark}
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.StaticReferences
import com.scustoms.services.MatchService.{MatchRole, ResolvedMatch}
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.QueueService._
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.scustoms.trueskill.RatingUtils.{percentageFormat, ratingFormat}
import com.typesafe.config.Config

import scala.concurrent.Future

class UserCommands(config: Config,
                   queueService: QueueService,
                   playerService: PlayerService,
                   matchService: MatchService
                  )(implicit client: DiscordClient) extends CommandController(client.requests) {

  import com.scustoms.Utils.StringImprovements

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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(InfoString))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit userCommandMessage => {
      val playerIdOpt = userCommandMessage.parsed
        .map(DiscordUtils.getUserIdFromMention)
        .getOrElse(Some(userCommandMessage.user.id))
      playerIdOpt match {
        case Some(playerId) =>
          OptFuture.fromFuture(playerService.findAndResolve(playerId)).flatMap {
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

  val LeaderboardString = "leaderboard"
  case class BestStatistics(conservativeRating: Double, meanRating: Double, role: MatchRole, player: PlayerWithStatistics)
  val leaderboard: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(LeaderboardString, "lederborde", "lederboard", "leatherboard"))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(playerService.getAllPlayers).flatMap {
        players =>
          val playerStrings = players
            .filter(_.totalGames > 5)
            .flatMap(p => p.getBestStatistics.map(bestStats => {
              val rating = bestStats._2.rating
              BestStatistics(rating.getConservativeRating, rating.getMean, bestStats._1, p)
            }))
            .sortBy(_.conservativeRating)
            .reverse
            .zipWithIndex
            .map { case (BestStatistics(cRating, mRating, role, player), index) =>
              val paddedIndex = (index + 1).toString.pad(indexPadding)
              val paddedUsername = player.gameUsername.pad(tablePadding)
              val paddedRole = role.toString.pad(shortTablePadding)
              val statistics = player.getRoleStatistics(role)
              val paddedWinRate = statistics.winRatePercentage.appended('%').pad(shortTablePadding)
              val paddedGamesPlayed = statistics.games.toString.pad(shortTablePadding)
              val paddedMRating = ratingFormat(mRating).pad(shortTablePadding)
              val paddedCRating = ratingFormat(cRating).pad(shortTablePadding)
              f"$paddedIndex$paddedUsername$paddedRole$paddedGamesPlayed$paddedWinRate$paddedMRating$paddedCRating"
            }
          val header = Seq(
            "#".pad(indexPadding),
            "Username".pad(tablePadding),
            "Role".pad(shortTablePadding),
            "# Games".pad(shortTablePadding),
            "Win rate".pad(shortTablePadding),
            "M. Rating".pad(shortTablePadding),
            "C. Rating".pad(shortTablePadding)
          ).reduceLeft(_ + _)
          val message = playerStrings.mkString(s"```$header\n\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    )

  val SuadosString = "suados"
  val suados: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(SuadosString))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(playerService.getAllPlayers).flatMap {
        players =>
          val playerStrings = players
            .filter(_.totalGames >= 10)
            .flatMap(p => p.getBestStatistics.map(bestStats =>
              (bestStats._2.games * 100.0 / p.totalGames, bestStats._1, bestStats._2, p)
            ))
            .filter(_._1 > 50)
            .sortBy(_._1)
            .reverse
            .zipWithIndex
            .map { case ((suor, role, statistics, player), index) =>
              val paddedIndex = (index + 1).toString.pad(indexPadding)
              val paddedUsername = player.gameUsername.pad(tablePadding)
              val paddedRole = role.toString.pad(shortTablePadding)
              val paddedWinRate = statistics.winRatePercentage.appended('%').pad(shortTablePadding)
              val paddedGamesPlayed = statistics.games.toString.pad(shortTablePadding)
              val paddedRating = percentageFormat(suor).appended('%').pad(tablePadding)
              f"$paddedIndex$paddedUsername$paddedRole$paddedGamesPlayed$paddedWinRate$paddedRating"
            }
          val header = Seq(
            "#".pad(indexPadding),
            "Username".pad(tablePadding),
            "Role".pad(shortTablePadding),
            "# Games".pad(shortTablePadding),
            "Win rate".pad(shortTablePadding),
            "Racio de suor".pad(tablePadding)
          ).reduceLeft(_ + _)
          val message = playerStrings.mkString(s"```$header\n\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    )

  val HistoryString = "history"
  val history: NamedComplexCommand[Option[Int], NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
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
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
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
  val join: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(JoinString))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit command => {
      val result: Future[Either[QueueError, QueuedPlayer]] = if (matchService.contains(command.user.id)) {
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
          queueService.upsertPlayer(queuedPlayer)
          val message = s"${command.user.mention} joined the queue (role: ${queuedPlayer.role}). Current queue size: ${queueService.queueSize}"
          DiscordUtils.reactAndRespond(positiveMark, message)
        case Left(error) =>
          DiscordUtils.reactAndRespond(negativeMark, error.message)
      }
    })

  val LeaveString = "leave"
  val leave: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(LeaveString))
    .asyncOpt(implicit command =>
      if (queueService.remove(command.user.id))
        DiscordUtils.reactAndRespond(positiveMark, s"${command.user.mention} left the game or watchers queue. Current queue size: ${queueService.queueSize}")
      else
        DiscordUtils.reactAndRespond(negativeMark, "You are not watching or in the queue")
    )

  val WatchString = "watch"
  val watch: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(WatchString))
    .asyncOpt(implicit command => {
      if (matchService.contains(command.user.id)) {
        DiscordUtils.reactAndRespond(negativeMark, "You are already in a match")
      } else {
        queueService.upsertWatcher(command.user.id)
        val message = s"${command.user.mention} joined the watchers. You will now be pulled into voice rooms on games."
        DiscordUtils.reactAndRespond(positiveMark, message)
      }
    })

  val ShowString = "show"
  val show: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
    .named(userCommandSymbols, Seq(ShowString))
    .withRequest(implicit m => {
      val allPlayersStrings = queueService.getQueue.zipWithIndex.map {
        case (QueuedPlayer(role, player), index) =>
          s"${(index + 1).toString.pad(indexPadding)}${player.gameUsername.pad(tablePadding)}${role.toString.pad(shortTablePadding)}"
      }
      val queueSize = s"Queue (${allPlayersStrings.length})".pad(tablePadding)
      val watchersSize = s"Watchers (${queueService.watchersSize})".pad(tablePadding)
      val preHeader = s"${"".pad(indexPadding)}$queueSize$watchersSize"
      val header = s"${"#".pad(indexPadding)}${"Username".pad(tablePadding)}${"Role".pad(tablePadding)}"
      val playersString = allPlayersStrings.mkString(s"```$preHeader\n$header\n\n", "\n", "```")
      m.textChannel.sendMessage(playersString)
    })

  val HelpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.onlyInTextRoom(StaticReferences.botChannel))
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

  val commandList = Seq(registered, history, watch, register, info, join, leave, show, leaderboard, help, suados)
}
