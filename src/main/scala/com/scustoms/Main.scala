package com.scustoms

import com.scustoms.bot.ScustomsBot
import com.scustoms.database.DatabaseManager
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
      val databaseManager = new DatabaseManager
      databaseManager.clearDatabase()
      System.exit(0)
    } else {
      new ScustomsBot(discordToken)
    }
  }
}
