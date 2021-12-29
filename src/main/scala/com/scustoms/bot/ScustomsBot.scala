package com.scustoms.bot

import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings, DiscordClient, OptFuture}
import com.scustoms.database.keepers.{MatchKeeper, PlayerKeeper, PlayerStatisticsKeeper}
import com.scustoms.database.{DatabaseManager, StaticReferences}
import com.scustoms.services.{MatchService, PlayerService, QueueService}
import com.typesafe.config.Config

import scala.concurrent.duration._
import scala.concurrent.Await

class ScustomsBot(discordToken: String, config: Config) {
  val clientSettings: ClientSettings = ClientSettings(discordToken)
  import clientSettings.executionContext

  var firstStart = true

  implicit val client: DiscordClient = Await.result(clientSettings.createClient(), 10.seconds)

  val databaseManager = new DatabaseManager
  //Await.result(databaseManager.clearDatabase(), 10.seconds)
  Await.result(databaseManager.setupDatabase(), 10.seconds)

  val playerKeeper = new PlayerKeeper(databaseManager)
  val playerStatisticsKeeper = new PlayerStatisticsKeeper(databaseManager)
  val matchKeeper = new MatchKeeper(databaseManager)

  val playerService = new PlayerService(playerKeeper, playerStatisticsKeeper)
  val queueService = new QueueService
  val matchService = new MatchService(matchKeeper, playerService)

  val userCommands = new UserCommands(config, queueService, playerService, matchService)
  val adminCommands = new AdminCommands(config, queueService, playerService, matchService)
  client.commands.bulkRunNamed(userCommands.commandList: _*)
  client.commands.bulkRunNamed(adminCommands.commandList: _*)

  client.onEventAsync { implicit c => {
    case APIMessage.Ready(_) =>
      println("Now ready")
      OptFuture.unit
    case APIMessage.GuildCreate(guild, _) if guild.id == StaticReferences.guildId & firstStart =>
      firstStart = false
      val channel = OptFuture.fromOption(StaticReferences.botChannel.resolve(StaticReferences.guildId))
      channel.map(channel => client.requestsHelper.run(channel.sendMessage("I'm back online.")))
  }}

  client.login()
}
