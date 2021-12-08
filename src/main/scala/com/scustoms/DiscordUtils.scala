package com.scustoms

import ackcord.{DiscordClient, OptFuture}
import ackcord.commands.GuildMemberCommandMessage
import ackcord.data.UserId
import ackcord.requests.CreateReaction
import ackcord.syntax.TextChannelSyntax

import scala.concurrent.ExecutionContext

object DiscordUtils {
  def tokenizeCommand(command: String): Array[String] = command.trim.tail.split(' ').tail

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
}
