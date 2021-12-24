package com.scustoms

import com.scustoms.bot.ScustomsBot
import com.scustoms.database.DatabaseManager
import com.typesafe.config.ConfigFactory

import scala.concurrent.Promise

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
      try {
        new ScustomsBot(discordToken)
      } catch {
        case err: Exception =>
          println(s"Start-up failure: ${err.getMessage}")
          System.exit(-1)
      }
    }
  }
}
