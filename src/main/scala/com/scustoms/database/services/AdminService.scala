package com.scustoms.database.services

import ackcord.CacheSnapshot
import ackcord.commands.UserCommandMessage
import ackcord.data.UserId
import ackcord.requests.CreateMessage
import ackcord.syntax.TextChannelSyntax
import akka.NotUsed

object AdminService {
  def isFromAdmin(id: UserId): Boolean = {
    id.toUnsignedLong == 138822865708515329L
  }

  def adminGate(command: UserCommandMessage[NotUsed])(f: => CreateMessage): CreateMessage = {
    command.user.discriminator
    if (isFromAdmin(command.user.id)) {
      f
    } else {
      implicit val c: CacheSnapshot = command.cache
      command.textChannel.sendMessage(s"Vai-te foder ${command.user.username}, não mandas em mim!")
    }
  }
}
