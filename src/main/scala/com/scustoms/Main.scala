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
      import com.scustoms.database.trueskill.RatingService.{calculator, gameInfo, teams}
      DatabaseService.clearDatabase().flatMap(_ => DatabaseService.setupDatabase())
      //println(s"Teams before: $teams")
      //println(s"Teams after: ${calculator.calculateNewRatings(gameInfo, teams, 1, 2)}")
      System.exit(0)
    } else
      new ScustomsBot(discordToken)
  }
}
