package com.scustoms

import ackcord.data.{SnowflakeType, TextChannel, TextChannelId, TextGuildChannel}
import ackcord.requests.CreateReaction
import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings, DiscordClient}

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

class ScustomsBot(discordToken: String) {
  val clientSettings: ClientSettings = ClientSettings(discordToken)
  val botChannel: TextChannelId = SnowflakeType[TextChannel](916070126611234816L)

  val client: DiscordClient = Await.result(clientSettings.createClient(), 10.seconds)
  implicit val ec: ExecutionContextExecutor = client.executionContext

  val botCommands = new BotCommands(client, shutdown)
  client.commands.bulkRunNamed(
    botCommands.hello, botCommands.info, botCommands.shutdown, botCommands.status, botCommands.react
  )

  client.onEventAsync { implicit c => {
    case APIMessage.Ready(cache) =>
      import client.requestsHelper._
      println("Now ready")
      for {
        channel <- optionPure(botChannel.resolve(cache.current))
        _ <- run(channel.sendMessage("Quem é vivo sempre aparece!"))
      } yield ()

    case APIMessage.MessageCreate(_, message, cache) if message.content.contains("good bot") =>
      client.requestsHelper.run(CreateReaction(message.channelId, message.id, ":kekw:")).map(_ => ())

    case APIMessage.ChannelCreate(_, channel: TextGuildChannel, _) =>
      client.requestsHelper.run(channel.sendMessage(content = "Não se aprende nada aqui")).map(_ => ())
  }}

  client.login()

  def shutdown(): Unit = {
    println("Shutting down in 3 seconds...")
    for {
      _ <- Future {Thread.sleep(3000)}
      _ <- client.logout()
    } yield System.exit(0)
  }
}
