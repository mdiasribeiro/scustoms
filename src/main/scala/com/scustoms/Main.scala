package com.scustoms

import com.typesafe.config.ConfigFactory

object Main extends App {
  val config = ConfigFactory.load()
  val discordToken = config.getString("scustoms.discordToken")
  val debugMode = config.getBoolean("scustoms.debugMode")

  //DatabaseService.testDatabase()

  if (discordToken.isBlank) {
    println("Discord token has not been set!")
    System.exit(-1)
  } else {
    new ScustomsBot(discordToken)
  }
}
