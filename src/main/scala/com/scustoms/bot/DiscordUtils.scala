package com.scustoms.bot

import ackcord.commands.GuildMemberCommandMessage
import ackcord.data.UserId
import ackcord.requests.CreateReaction
import ackcord.syntax.TextChannelSyntax
import ackcord.{CacheSnapshot, DiscordClient, OptFuture}
import com.scustoms.services.MatchService

import scala.concurrent.ExecutionContext
import com.scustoms.Utils.StringImprovements

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
        val ratingStr = player.niceString(Some(role))
        s"${player.gameUsername.pad(columnSize)}${role.toString.pad(columnSize)}${ratingStr.pad(columnSize)}"
    }
  }

  def ongoingMatchToString(m: MatchService.OngoingMatch, teamA: String, teamB: String, columnSize: Int)(implicit c: CacheSnapshot): String = {
    val header = s"${"Username".pad(columnSize)}${"Role".pad(columnSize)}${"Rating".pad(columnSize)}"
    val teamAPlayers = playersToStrings(m.team1, columnSize).mkString(s"\n$teamA\n$header\n", "\n", "")
    val teamBPlayers = playersToStrings(m.team2, columnSize).mkString(s"\n$teamB\n$header\n", "\n", "")
    val quality = f"${m.quality * 100}%3.02f"
    s"```Match probability of draw: $quality%\n$teamAPlayers\n$teamBPlayers```"
  }

  def parseWinningTeamA(parsed: Int): Option[Boolean] = parsed match {
    case 1 => Some(true)
    case 2 => Some(false)
    case _ => None
  }
}
