package com.scustoms.bot

import ackcord.commands.{CommandError, CommandFunction, GuildCommandMessage, GuildMemberCommandMessage}
import ackcord.data.{Guild, GuildMember, RoleId, TextGuildChannelId, UserId}
import ackcord.requests.CreateReaction
import ackcord.syntax.TextChannelSyntax
import ackcord.{CacheSnapshot, DiscordClient, OptFuture}
import akka.NotUsed
import akka.stream.scaladsl.Flow
import com.scustoms.services.MatchService

import scala.concurrent.ExecutionContext
import com.scustoms.Utils.StringImprovements
import com.scustoms.Utils.SeqImprovements
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.trueskill.RatingUtils.percentageFormat

object DiscordUtils {
  def getUserIdFromMention(mention: String): Option[UserId] = {
    Option.when(mention.startsWith("<@") && mention.endsWith(">")) {
      if (mention.startsWith("<@!")) {
        UserId(mention.slice(3, mention.length - 1))
      } else {
        UserId(mention.slice(2, mention.length - 1))
      }
    }
  }

  def reactAndRespond[T](emoji: String, response: String)
                        (implicit client: DiscordClient, command: GuildMemberCommandMessage[T], ec: ExecutionContext): OptFuture[Unit] = {
    val react = CreateReaction(command.textChannel.id, command.message.id, emoji)
    val respond = command.textChannel.sendMessage(response)
    client.requestsHelper.runMany(react, respond)(command.cache).map(_ => ())
  }

  def playersToStrings(team: MatchService.MatchTeam, columnSize: Int)(implicit c: CacheSnapshot): Seq[String] = {
    team.seq.map {
      case MatchService.MatchPlayer(role, player) =>
        val username = player.gameUsername.pad(columnSize)
        val roleStr = role.toString.pad(columnSize)
        val meanRatingStr = player.meanRatingToString(role).pad(columnSize)
        val conservativeRatingStr = player.conservativeRatingToString(role).pad(columnSize)
        s"$username$roleStr$meanRatingStr$conservativeRatingStr"
    }
  }

  def ongoingMatchToString(m: MatchService.OngoingMatch, teamA: String, teamB: String, columnSize: Int)(implicit c: CacheSnapshot): String = {
    val header = s"${"Role".pad(columnSize)}${"M. Rating".pad(columnSize)}${"C. Rating".pad(columnSize)}"
    val teamAPlayers = playersToStrings(m.team1, columnSize).mkString(s"\n${teamA.pad(columnSize)}$header\n", "\n", "")
    val teamBPlayers = playersToStrings(m.team2, columnSize).mkString(s"\n${teamB.pad(columnSize)}$header\n", "\n", "")
    val quality = percentageFormat(m.quality * 100)
    s"```Match quality: $quality%\n$teamAPlayers\n$teamBPlayers```"
  }

  def playerToString(p: PlayerWithStatistics, tablePadding: Int): String = {
    val header = Seq("Role", "# Games", "Win rate", "M. Rating", "C. Rating").padConcat(tablePadding)
    val playerStats = Seq((MatchService.Top, p.topStats), (MatchService.Jungle, p.jungleStats), (MatchService.Mid, p.midStats), (MatchService.Bot, p.botStats), (MatchService.Support, p.supportStats)).map {
      case (role, stats) =>
        Seq(role.toString, stats.games.toString, stats.winRatePercentage, stats.formattedMeanRating,
          stats.formattedConservativeRating).padConcat(tablePadding)
    }.mkString("\n")
    s"```In-game name: ${p.gameUsername}\n\n$header\n$playerStats```".stripMargin
  }

  def parseWinningTeamA(parsed: Int): Option[Boolean] = parsed match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }

  private def hasRole(user: GuildMember, guild: Guild, role: RoleId): Boolean = {
    val ownerId = guild.ownerId
    if (user.userId == ownerId) true
    else {
      user.roleIds.contains(role)
    }
  }

  def needRole[M[A] <: GuildCommandMessage[A]](neededRole: RoleId): CommandFunction[M, M] = new CommandFunction[M, M] {
    override def flow[A]: Flow[M[A], Either[Option[CommandError], M[A]], NotUsed] =
      Flow[M[A]].map { m =>
        val guild = m.guild

        val allowed = guild.members
          .get(UserId(m.message.authorId))
          .exists(usr => hasRole(usr, guild, neededRole))

        if (allowed) Right(m)
        else Left(Some(CommandError(s"You do not have the required role to use this command", m.textChannel, m.cache)))
      }
  }

  def onlyInTextRoom[M[A] <: GuildCommandMessage[A]](allowedTextRoom: TextGuildChannelId): CommandFunction[M, M] = new CommandFunction[M, M] {
    override def flow[A]: Flow[M[A], Either[Option[CommandError], M[A]], NotUsed] =
      Flow[M[A]].map { m =>
        if (m.textChannel.id == allowedTextRoom)
          Right(m)
        else
          Left(None)
      }
  }
}
