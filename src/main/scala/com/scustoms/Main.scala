package com.scustoms

import com.scustoms.bot.ScustomsBot
import com.typesafe.config.ConfigFactory

object Main extends App {
  val config = ConfigFactory.load()
  val discordToken = config.getString("scustoms.discordToken")

  if (discordToken.isBlank) {
    println("Discord token has not been set!")
    System.exit(-1)
  } else {
    try {
      new ScustomsBot(discordToken, config)
    } catch {
      case err: Exception =>
        println(s"Start-up failure: ${err.getMessage}")
        System.exit(-1)
    }
  }
}
