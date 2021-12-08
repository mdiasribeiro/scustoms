package com.scustoms.database.services

import ackcord.{CacheSnapshot, DiscordClient, OptFuture}
import ackcord.commands.GuildMemberCommandMessage
import ackcord.syntax.TextChannelSyntax
import com.scustoms.database.StaticReferences

import scala.concurrent.ExecutionContext

object AdminService {
  def adminGate[T](command: GuildMemberCommandMessage[T])(f: => OptFuture[Unit])
               (implicit client: DiscordClient, ec: ExecutionContext): OptFuture[Unit] = {
    if (command.guildMember.hasRoleAboveId(command.guild, Seq(StaticReferences.adminRoleId))) {
      f
    } else {
      implicit val c: CacheSnapshot = command.cache
      val message = s"Sorry ${command.user.mention}, you lack sufficient permissions to use that command."
      client.requestsHelper.run(command.textChannel.sendMessage(message)).map(_ => ())
    }
  }
}
