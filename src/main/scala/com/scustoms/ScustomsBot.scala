package com.scustoms

import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings, DiscordClient, OptFuture}
import com.scustoms.database.StaticReferences

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

class ScustomsBot(discordToken: String) {
  val clientSettings: ClientSettings = ClientSettings(discordToken)

  val client: DiscordClient = Await.result(clientSettings.createClient(), 10.seconds)
  val queueService = new QueueService
  val matchService = new MatchmakingService
  implicit val ec: ExecutionContextExecutor = client.executionContext

  val userCommands = new UserCommands(client, queueService, matchService)
  val adminCommands = new AdminCommands(client, queueService, matchService)
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
