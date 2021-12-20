package com.scustoms.bot

import ackcord.commands.GuildMemberCommandMessage
import ackcord.data.UserId
import ackcord.requests.CreateReaction
import ackcord.syntax.TextChannelSyntax
import ackcord.{CacheSnapshot, DiscordClient, OptFuture}
import com.scustoms.services.MatchmakingService

import scala.concurrent.ExecutionContext

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

  def playersToStrings(team: Seq[MatchmakingService.MatchPlayer])(implicit c: CacheSnapshot): Seq[String] = {
    team.map {
      case MatchmakingService.MatchPlayer(discordId, role, dbPlayer) =>
        val mention = discordId.resolve.map(_.mention).getOrElse(dbPlayer.discordUsername)
        val ratingStr = dbPlayer.niceString(role)
        s"$mention: ${dbPlayer.gameUsername}, role: $role, $ratingStr"
    }
  }

  def matchToString(m: MatchmakingService.Match, teamA: String, teamB: String)(implicit c: CacheSnapshot): String = {
    val teamAPlayers = playersToStrings(m.teamA).mkString(s"$teamA\n", "\n", "")
    val teamBPlayers = playersToStrings(m.teamB).mkString(s"$teamB\n", "\n", "")
    f"```Match probability of draw: ${m.quality}%1.2f\n$teamAPlayers\n$teamBPlayers```"
  }
}
