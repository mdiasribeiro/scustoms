package com.scustoms.bot

import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings, DiscordClient, OptFuture}
import com.scustoms.database.keepers.{MatchKeeper, PlayerKeeper, PlayerStatisticsKeeper}
import com.scustoms.database.{DatabaseManager, StaticReferences}
import com.scustoms.services.{MatchService, MatchmakingService, PlayerService, QueueService}

import scala.concurrent.duration._
import scala.concurrent.Await

class ScustomsBot(discordToken: String) {
  val clientSettings: ClientSettings = ClientSettings(discordToken)
  import clientSettings.executionContext

  val client: DiscordClient = Await.result(clientSettings.createClient(), 10.seconds)

  val databaseManager = new DatabaseManager
  //Await.result(databaseManager.clearDatabase(), 10.seconds)
  Await.result(databaseManager.setupDatabase(), 10.seconds)

  val playerKeeper = new PlayerKeeper(databaseManager)
  val playerStatisticsKeeper = new PlayerStatisticsKeeper(databaseManager)
  val matchKeeper = new MatchKeeper(databaseManager)

  val playerService = new PlayerService(playerKeeper, playerStatisticsKeeper)
  val queueService = new QueueService(playerService)
  val matchmakingService = new MatchmakingService(matchKeeper)
  val matchService = new MatchService(matchKeeper, playerKeeper)

  val userCommands = new UserCommands(client, queueService, playerService, matchService)
  val adminCommands = new AdminCommands(client, queueService, playerService, matchmakingService, matchService)
  client.commands.bulkRunNamed(userCommands.commandList: _*)
  client.commands.bulkRunNamed(adminCommands.commandList: _*)

  client.onEventAsync { implicit c => {
    case APIMessage.Ready(_) =>
      println("Now ready")
      OptFuture.unit
    case APIMessage.GuildCreate(guild, _) if guild.id == StaticReferences.guildId =>
      val channel = OptFuture.fromOption(StaticReferences.botChannel.resolve(StaticReferences.guildId))
      channel.map(channel => client.requestsHelper.run(channel.sendMessage("I'm back online.")))
    case another =>
      println(s"New event of type: ${another.getClass.getSimpleName}")
      OptFuture.unit
  }}

  client.login()
}
