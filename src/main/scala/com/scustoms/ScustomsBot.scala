package com.scustoms

import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings, DiscordClient, OptFuture}
import com.scustoms.database.StaticReferences

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

class ScustomsBot(discordToken: String) {
  val clientSettings: ClientSettings = ClientSettings(discordToken)

  val client: DiscordClient = Await.result(clientSettings.createClient(), 10.seconds)
  implicit val ec: ExecutionContextExecutor = client.executionContext

  val botCommands = new BotCommands(client, shutdown)
  client.commands.bulkRunNamed(
    botCommands.hello, botCommands.info, botCommands.shutdown, botCommands.register, botCommands.join,
    botCommands.show, botCommands.clear, botCommands.leave, botCommands.add, botCommands.remove, botCommands.start
  )

  client.onEventAsync { implicit c => {
    case APIMessage.Ready(_) =>
      println("Now ready")
      OptFuture.unit
    case APIMessage.GuildCreate(guild, _) if guild.id == StaticReferences.guildId =>
      val channel = OptFuture.fromOption(StaticReferences.botChannel.resolve(StaticReferences.guildId))
      channel.map(channel => client.requestsHelper.run(channel.sendMessage("I'm back online")))
    case another =>
      println(s"New event of type: ${another.getClass.getSimpleName}")
      OptFuture.unit
  }}

  client.login()

  def shutdown(): Unit = {
    println("Shutting down in 5 seconds...")
    for {
      _ <- Future {Thread.sleep(5000)}
      _ <- client.shutdownAckCord()
    } yield System.exit(0)
  }
}
