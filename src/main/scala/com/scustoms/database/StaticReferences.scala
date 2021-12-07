package com.scustoms.database

import ackcord.data.{Guild, GuildId, Role, RoleId, SnowflakeType, TextGuildChannel, TextGuildChannelId, User, UserId}

object StaticReferences {
  val guildId: GuildId = SnowflakeType[Guild](149979917570801665L)
  val botChannel: TextGuildChannelId = SnowflakeType[TextGuildChannel](916070126611234816L)
  val adminId: UserId = SnowflakeType[User](149979917570801665L)
  val roleId: RoleId = SnowflakeType[Role](916068561603158106L)
}
