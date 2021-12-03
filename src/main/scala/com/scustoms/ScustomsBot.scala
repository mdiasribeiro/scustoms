package com.scustoms

import ackcord.data.TextGuildChannel
import ackcord.syntax.TextChannelSyntax
import ackcord.{APIMessage, ClientSettings, DiscordClient}

import scala.concurrent.Await
import scala.concurrent.duration._

class ScustomsBot(discordToken: String) {
  val clientSettings: ClientSettings = ClientSettings(discordToken)
  val botChannel = "scustoms"

  val client: DiscordClient = Await.result(clientSettings.createClient(), 10.seconds)

  val myCommands = new BotCommands(client.requests)
  client.commands.bulkRunNamed(myCommands.hello, myCommands.info)

  client.onEventSideEffectsIgnore {
    case APIMessage.Ready(_) => println("Now ready")
  }

  client.onEventSideEffects { c => {
    case APIMessage.MessageCreate(_, message, _) => println(c.getTextChannel(message.channelId))
  }}

  client.onEventAsync { implicit c => {
    case APIMessage.ChannelCreate(_, channel: TextGuildChannel, _) =>
      client.requestsHelper.run(channel.sendMessage(content = "Não se aprende nada aqui")).map(_ => ())(client.executionContext)
  }}

  client.login()
}
