package com.scustoms.database

import ackcord.data._

object StaticReferences {
  val guildId: GuildId = SnowflakeType[Guild](149979917570801665L)
  val botChannel: TextGuildChannelId = SnowflakeType[TextGuildChannel](916070126611234816L)
  val teamAChannel: VoiceGuildChannelId = SnowflakeType[VoiceGuildChannel](149979918355267584L)
  val teamBChannel: VoiceGuildChannelId = SnowflakeType[VoiceGuildChannel](168517644092178432L)
  val lobbyChannel: VoiceGuildChannelId = teamAChannel
  val adminId: UserId = SnowflakeType[User](149979917570801665L)
  val customsRoleId: RoleId = SnowflakeType[Role](916068561603158106L)
  val managerRoleId: RoleId = SnowflakeType[Role](925340105919586304L)
  val adminRoleId: RoleId = SnowflakeType[Role](149982815004393472L)
}
