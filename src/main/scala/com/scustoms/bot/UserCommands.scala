package com.scustoms.bot

import ackcord.commands.{CommandController, MessageParser, NamedCommand, NamedComplexCommand}
import ackcord.requests.{AddGuildMemberRole, CreateReaction}
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, OptFuture}
import akka.NotUsed
import Emojis.{negativeMark, positiveMark}
import com.scustoms.database.keepers.PlayerKeeper
import com.scustoms.database.StaticReferences
import com.scustoms.database.keepers.PlayerKeeper.PlayerNotFound
import com.scustoms.services.MatchmakingService.CompleteMatch
import com.scustoms.services.QueueService._
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future

class UserCommands(client: DiscordClient,
                   queueService: QueueService,
                   playerService: PlayerService,
                   matchService: MatchService
                  ) extends CommandController(client.requests) {
  implicit val c: DiscordClient = client

  val config: Config = ConfigFactory.load()
  val userCommandSymbols = Seq(config.getString("scustoms.userCommandSymbol"))
  val tablePadding: Int = config.getInt("scustoms.tablePadding")

  val hello: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("hello"))
    .withRequest(m => m.textChannel.sendMessage(s"Hello ${m.user.username}!"))

  val info: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("info"))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(playerService.find(userCommandMessage.user.id)).flatMap {
        case Some(player) =>
          val header = Seq("Role", "Games played", "Win rate", "Rating").map(_.padTo(tablePadding, ' ')).reduceLeft(_ + _)
          val top = Seq("Top", player.top.games.toString, player.top.winRatePercentage, player.top.formattedRating).map(_.padTo(tablePadding, ' ')).reduceLeft(_ + _)
          val jungle = Seq("Jungle", player.jungle.games.toString, player.jungle.winRatePercentage, player.jungle.formattedRating).map(_.padTo(tablePadding, ' ')).reduceLeft(_ + _)
          val mid = Seq("Mid", player.mid.games.toString, player.mid.winRatePercentage, player.mid.formattedRating).map(_.padTo(tablePadding, ' ')).reduceLeft(_ + _)
          val bot = Seq("Bot", player.bot.games.toString, player.bot.winRatePercentage, player.bot.formattedRating).map(_.padTo(tablePadding, ' ')).reduceLeft(_ + _)
          val support = Seq("Support", player.support.games.toString, player.support.winRatePercentage, player.support.formattedRating).map(_.padTo(tablePadding, ' ')).reduceLeft(_ + _)
          val message =
            s"""${userCommandMessage.user.mention}
              |```
              |In-game name: ${player.gameUsername}\n
              |$header\n$top\n$jungle\n$mid\n$bot\n$support
              |```""".stripMargin
          DiscordUtils.reactAndRespond(positiveMark, message)
        case None =>
          DiscordUtils.reactAndRespond(negativeMark, "Player not found. Make sure you have registered first.")
      }
    )

  val leaderboard: NamedCommand[NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("leaderboard"))
    .asyncOpt(implicit userCommandMessage =>
      OptFuture.fromFuture(playerService.getAllPlayers).flatMap {
        players =>
          val playerStrings = players
            .map(p => {
              val bestStats = p.getTopStatistics
              (bestStats._2.rating.getConservativeRating, bestStats._1, bestStats._2, p)
            })
            .sortBy(_._1)
            .map { case (rating, role, statistics, player) =>
              val paddedUsername = player.gameUsername.padTo(tablePadding, ' ')
              val paddedRole = role.toString.padTo(tablePadding, ' ')
              val paddedWinRate = statistics.winRatePercentage.appended('%').padTo(tablePadding, ' ')
              val paddedGamesPlayed = statistics.games.toString.padTo(tablePadding, ' ')
              val paddedRating = f"$rating%1.2f".padTo(tablePadding, ' ')
              f"$paddedUsername$paddedRole$paddedGamesPlayed$paddedWinRate$paddedRating"
            }
          val header = Seq("Username", "Best role", "Games played", "Win rate", "Rating")
            .map(_.padTo(tablePadding, ' '))
            .reduceLeft(_ + _)
          val message = playerStrings.mkString(s"```$header\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
      }
    )

  val history: NamedComplexCommand[Option[Int], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("history"))
    .parsing[Option[Int]](MessageParser.optional)
    .asyncOpt(implicit userCommandMessage => {
      val historySize = userCommandMessage.parsed.map(c => math.min(100, math.max(c, 0))).getOrElse(10)
      OptFuture.fromFuture(matchService.getLastN(historySize)).flatMap {
        case Right(matches) =>
          val matchStrings = matches.map {
            case CompleteMatch(team1Won, team1, team2) =>
              val team1String = team1.map(_.gameUsername.padTo(tablePadding, ' '))
              val team2String = team2.map(_.gameUsername.padTo(tablePadding, ' '))
              val winner = if (team1Won) "Team 1".padTo(tablePadding, ' ') else "Team 2".padTo(tablePadding, ' ')
              f"$winner$team1String vs $team2String"
          }
          val header = s"${"Winner".padTo(tablePadding, ' ')}${"Team 1".padTo(tablePadding * 5, ' ')} vs ${"Team 2".padTo(tablePadding * 5, ' ')}"
          val message = matchStrings.mkString(s"```$header\n", "\n", "```")
          DiscordUtils.reactAndRespond(positiveMark, message)
        case Left(err) =>
          DiscordUtils.reactAndRespond(negativeMark, s"Error: ${err.toString}")
      }
    })

  val register: NamedComplexCommand[String, NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("register"))
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

  val join: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("join"))
    .parsing[Option[String]](MessageParser.optional)
    .asyncOpt(implicit command => {
      val result: Future[Either[QueueError, QueuedPlayer]] = playerService.exists(command.user.id).map {
        case false =>
          Left(PlayerDoesNotExist)
        case true if queueService.contains(command.user.id) =>
          Left(PlayerAlreadyInQueue)
        case true if command.parsed.isDefined =>
          command.parsed
            .map(QueueService.parseRole)
            .toRight(ErrorParsingRole)
            .map(role => QueuedPlayer(command.user.id, role))
        case true =>
          Right(QueuedPlayer(command.user.id, None))
      }

      OptFuture.fromFuture(result).map {
        case Right(queuedPlayer) =>
          queueService.add(queuedPlayer)
          val message = s"${command.user.mention} joined the queue (role: ${queuedPlayer.role.getOrElse("Fill")}). Current queue size: ${queueService.length}"
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
      OptFuture.fromFuture(queueService.extendedInfo).map {
        case Right(allPlayers) =>
            val niceString = allPlayers.map {
              case ExtendedQueuedPlayer(_, Some(role), dbPlayer) =>
                val ratingStr = dbPlayer.niceString(role)
                s"Username: ${dbPlayer.gameUsername}, queued role: $role, $ratingStr"
              case ExtendedQueuedPlayer(_, None, dbPlayer) =>
                s"Username: ${dbPlayer.gameUsername}, queued role: Fill"
            }.mkString(s"```Queue (${allPlayers.length})\n", "\n", "```")
            m.textChannel.sendMessage(niceString)
        case Left(PlayerNotFound) =>
          m.textChannel.sendMessage("Error: A player was not found in the database")
        case Left(_) =>
          m.textChannel.sendMessage("An unknown error has occurred")
      }
    )

  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .named(userCommandSymbols, Seq("help"))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(implicit m => {
      val symbolStr = userCommandSymbols.head
      val helpText = m.parsed.map(_.toLowerCase) match {
        case Some("hello") =>
          s"""```
             |The bot will say hello to you. He's nice like that!
             |Usage: ${symbolStr}hello
             |```""".stripMargin
        case Some("history") =>
          s"""```
             |Look into the N latest matches.
             |Usage: ${symbolStr}history <?N>
             |N will be capped within the range [0, 100]
             |```""".stripMargin
        case Some("register") =>
          s"""```
             |Register yourself in the database (required to participate)
             |Usage: ${symbolStr}register <in_game_username_>
             |```""".stripMargin
        case Some("info") =>
          s"""```
             |Shows player information
             |Usage: ${symbolStr}info <?discord_mention>
             |```""".stripMargin
        case Some("join") =>
          s"""```
             |Join the game queue
             |Usage: ${symbolStr}join <?role>
             |(Possible roles: top, jungle, mid, bot, sup/support)
             |```""".stripMargin
        case Some("leave") =>
          s"""```
             |Leave the game queue
             |Usage: ${symbolStr}leave
             |```""".stripMargin
        case Some("show") =>
          s"""```
             |Show current queue state
             |Usage: ${symbolStr}show
             |```""".stripMargin
        case Some("leaderboard") =>
          s"""```
             |Show the leaderboard
             |Usage: ${symbolStr}leaderboard
             |```""".stripMargin
        case _ =>
          s"""```User command list:
             |Hello        The bot will say hello to you     (${symbolStr}hello)
             |History      Look into the N latest matches    (${symbolStr}history <?N>)
             |Register     Register yourself in the database (${symbolStr}register <in_game_username>)
             |Info         Shows player information          (${symbolStr}info <?discord_mention>)
             |Join         Join the game queue               (${symbolStr}join <?role>)
             |Leave        Leave the game queue              (${symbolStr}leave)
             |Show         Show current queue state          (${symbolStr}show)
             |Leaderboard  Show the leaderboard              (${symbolStr}leaderboard)
             |
             |For more details say ${symbolStr}help <command>
             |```""".stripMargin
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(hello, history, register, info, join, leave, show, leaderboard, help)
}
