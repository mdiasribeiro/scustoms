package com.scustoms

import com.scustoms.database.DatabaseService
import com.typesafe.config.ConfigFactory

object Main extends App {
  val config = ConfigFactory.load()
  val discordToken = config.getString("scustoms.discordToken")
  val debugMode = config.getBoolean("scustoms.debugMode")

  if (discordToken.isBlank) {
    println("Discord token has not been set!")
    System.exit(-1)
  } else {
    if (debugMode) {
      import DatabaseService.ec
      DatabaseService.clearDatabase().flatMap(_ => DatabaseService.setupDatabase())
      System.exit(0)
    } else
      new ScustomsBot(discordToken)
  }
}
