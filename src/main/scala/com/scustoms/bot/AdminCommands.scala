package com.scustoms.bot

import ackcord.commands._
import ackcord.data.RoleId
import ackcord.syntax.TextChannelSyntax
import ackcord.{DiscordClient, OptFuture}
import akka.NotUsed
import Emojis.positiveMark
import com.scustoms.database.StaticReferences
import com.scustoms.services.MatchService.OngoingMatch
import com.scustoms.services.{MatchService, PlayerService}
import com.scustoms.trueskill.RatingUtils
import com.typesafe.config.Config

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class AdminCommands(config: Config,
                    playerService: PlayerService,
                    matchService: MatchService
                   )(implicit client: DiscordClient) extends CommandController(client.requests) {

  import com.scustoms.Utils.StringImprovements

  val adminCommandSymbols = Seq(config.getString("scustoms.adminCommandSymbol"))
  val tablePadding: Int = config.getInt("scustoms.tablePadding")
  val requiredRole: RoleId = StaticReferences.adminRoleId

  final val ShutdownString = "shutdown"
  val shutdown: NamedComplexCommand[String, NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(adminCommandSymbols, Seq(ShutdownString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .parsing[String]
    .asyncOpt(implicit m => {
      println("Shutting down in 3 seconds...")
      for {
        _ <- Future {Thread.sleep(3000)}
        _ <- client.shutdownAckCord()
      } yield System.exit(0)
      client.requestsHelper.run(m.textChannel.sendMessage("Shutting down in 3 seconds...")).map(_ => ())
    })

  final val ResetString = "resetRatings"
  val reset: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(adminCommandSymbols, Seq(ResetString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => OptFuture
      .fromFuture(playerService.resetRating)
      .map(_ => DiscordUtils.reactAndRespond(positiveMark, s"Database ratings have been reset"))
    )

  final val ReprocessString = "reprocess"
  val reprocess: NamedCommand[NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(adminCommandSymbols, Seq(ReprocessString))
    .andThen(DiscordUtils.needRole(requiredRole))
    .asyncOpt(implicit m => {
      val limit = 100
      var offset = 0
      var hasMore = true
      Await.result(playerService.resetRating, 10.seconds)
      println("Ratings have been reset.")
      while (hasMore) {
        println(s"Offset: $offset")
        val iteration = matchService
          .getUnresolved(limit, offset)
          .map(batch => {
            println(s"Processing a new match batch of ${batch.length} matches...")
            println(s"Batch length: ${batch.length}")
            hasMore = batch.length >= limit
            offset = offset + batch.length
            batch.foreach(unresolvedMatch => {
              val processedMatch = matchService.resolveStoredMatch(unresolvedMatch).flatMap {
                case Some(resolvedStoredMatch) =>
                  val ongoingMatch = OngoingMatch.fromResolvedStoredMatch(resolvedStoredMatch)
                  val completeMatch = RatingUtils.calculate(ongoingMatch, resolvedStoredMatch.team1Won)
                  matchService.updateRatings(completeMatch)
                case None =>
                  println("Error: unresolved match!")
                  Future.successful(())
              }
              Await.result(processedMatch, 10.seconds)
            })
            println(s"Batch processed.")
          })
        Await.result(iteration, 10.seconds)
      }
      DiscordUtils.reactAndRespond(positiveMark, s"Match re-processing complete!")
    })

  val helpString = "help"
  val help: NamedComplexCommand[Option[String], NotUsed] = GuildCommand
    .andThen(DiscordUtils.allowedTextRoom(StaticReferences.botChannel))
    .named(adminCommandSymbols, Seq(helpString))
    .parsing[Option[String]](MessageParser.optional)
    .withRequest(m => {
      val symbolStr = adminCommandSymbols.head
      val helpText = m.parsed match {
        case Some(ShutdownString) =>
          s"""```
             |Shuts the bot down
             |Usage: $symbolStr$ShutdownString -a
             |```""".stripMargin
        case Some(ResetString) =>
          s"""```
             |Reset all players rating to default values
             |Usage: $symbolStr$ResetString
             |```""".stripMargin
        case Some(ReprocessString) =>
          s"""```
             |Resets the ratings and reprocess all matches in the database
             |Usage: $symbolStr$ReprocessString
             |```""".stripMargin
        case _ =>
          val reset = s"$symbolStr$ResetString".pad(tablePadding)
          val reprocess = s"$symbolStr$ReprocessString".pad(tablePadding)
          val shutdown = s"$symbolStr$ShutdownString".pad(tablePadding)
          Seq(
            s"""$reset Reset all players rating to default values""",
            s"""$reprocess Reprocess the matches in the database""",
            s"""$shutdown Shuts the bot down"""
          ).mkString("```Admin command list:\n\n", "\n", s"\n\nFor more details, say ${symbolStr}help <command>```")
      }
      m.textChannel.sendMessage(helpText)
    })

  val commandList = Seq(shutdown, help, reset, reprocess)
}
