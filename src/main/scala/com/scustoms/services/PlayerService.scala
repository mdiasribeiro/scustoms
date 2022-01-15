package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.Utils
import com.scustoms.database.DatabaseManager._
import com.scustoms.database.keepers.PlayerKeeper.StoredPlayer
import com.scustoms.database.keepers.PlayerStatisticsKeeper.PlayerStatistics
import com.scustoms.database.keepers.{PlayerKeeper, PlayerStatisticsKeeper}
import com.scustoms.services.MatchService.{MatchPlayer, MatchRole}
import com.scustoms.trueskill.RatingUtils
import de.gesundkrank.jskills.Rating

import scala.concurrent.{ExecutionContext, Future}

object PlayerService {
  case class PlayerWithStatistics(discordId: UserId, discordUsername: String, gameUsername: String,
                                  topStats: PlayerStatistics, jungleStats: PlayerStatistics, midStats: PlayerStatistics,
                                  botStats: PlayerStatistics, supportStats: PlayerStatistics) {

    def getStatisticsBy(minGames: Int = 0)(indexer: PlayerStatistics => Double): Option[(MatchRole, PlayerStatistics)] = {
      val stats = Seq(
        Option.when(topStats.games >= minGames)     ((indexer(topStats),     (MatchService.Top, topStats))),
        Option.when(jungleStats.games >= minGames)  ((indexer(jungleStats),  (MatchService.Jungle, jungleStats))),
        Option.when(midStats.games >= minGames)     ((indexer(midStats),     (MatchService.Mid, midStats))),
        Option.when(botStats.games >= minGames)     ((indexer(botStats),     (MatchService.Bot, botStats))),
        Option.when(supportStats.games >= minGames) ((indexer(supportStats), (MatchService.Support, supportStats))),
      ).flatten
      Option.unless(stats.isEmpty) {
        stats.maxBy(_._1)._2
      }
    }

    def getRoleStatistics(role: MatchRole): PlayerStatistics = role match {
      case MatchService.Top     => topStats
      case MatchService.Jungle  => jungleStats
      case MatchService.Mid     => midStats
      case MatchService.Bot     => botStats
      case MatchService.Support => supportStats
    }

    def meanRatingToString(role: MatchRole): String =
      f"${getRoleStatistics(role).rating.getMean}%03.02f"

    def conservativeRatingToString(role: MatchRole): String =
      f"${getRoleStatistics(role).rating.getConservativeRating}%03.02f"

    def totalGames: Long = topStats.games + jungleStats.games + midStats.games + botStats.games + supportStats.games

    def totalWins: Long = topStats.wins + jungleStats.wins + midStats.wins + botStats.wins + supportStats.wins

    def toStoredPlayer: StoredPlayer = StoredPlayer(0L, discordId, discordUsername, gameUsername,
      topStats.id, jungleStats.id, midStats.id, botStats.id, supportStats.id)

    def toMatchPlayer(role: MatchRole): MatchPlayer = MatchPlayer(role, this)

    def updatedRating(role: MatchRole, newRating: Rating, won: Boolean): PlayerWithStatistics =
      role match {
        case MatchService.Top     => this.copy(topStats = topStats.updated(newRating, won))
        case MatchService.Jungle  => this.copy(jungleStats = jungleStats.updated(newRating, won))
        case MatchService.Mid     => this.copy(midStats = midStats.updated(newRating, won))
        case MatchService.Bot     => this.copy(botStats = botStats.updated(newRating, won))
        case MatchService.Support => this.copy(supportStats = supportStats.updated(newRating, won))
      }
  }
}

class PlayerService(playerKeeper: PlayerKeeper, playerStatisticsKeeper: PlayerStatisticsKeeper)(implicit ec: ExecutionContext) {
  import PlayerService._

  def insert(discordId: UserId, discordUsername: String, gameUsername: String): Future[Either[DatabaseError, Long]] =
    exists(discordId).flatMap {
      case false =>
        for {
          topTry <- playerStatisticsKeeper.insert(discordId)
          junTry <- playerStatisticsKeeper.insert(discordId)
          midTry <- playerStatisticsKeeper.insert(discordId)
          botTry <- playerStatisticsKeeper.insert(discordId)
          supTry <- playerStatisticsKeeper.insert(discordId)
          insertResult = for {
            top <- topTry
            jun <- junTry
            mid <- midTry
            bot <- botTry
            sup <- supTry
          } yield playerKeeper.insert(StoredPlayer(0L, discordId, discordUsername, gameUsername, top, jun, mid, bot, sup))
          foldedFuture <- Utils.foldEitherOfFuture(insertResult)
        } yield foldedFuture.flatten
      case true =>
        Future.successful(Left(PlayerAlreadyExists))
    }

  def update(statistics: PlayerStatistics): Future[Boolean] =
    playerStatisticsKeeper.update(statistics)

  def updateGameUsername(playerId: UserId, username: String): Future[Either[DatabaseError, Boolean]] =
    exists(playerId).flatMap {
      case true =>
        playerKeeper.updateGameUsername(playerId, username).map(Right(_))
      case false =>
        Future.successful(Left(PlayerNotFound))
    }

  def exists(discordId: UserId): Future[Boolean] = playerKeeper.exists(discordId)

  def resolvePlayer(player: PlayerKeeper.StoredPlayer): Future[Either[DatabaseError, PlayerWithStatistics]] =
    for {
      topResult <- playerStatisticsKeeper.find(player.topStatsId)
      jungleResult <- playerStatisticsKeeper.find(player.jungleStatsId)
      midResult <- playerStatisticsKeeper.find(player.midStatsId)
      botResult <- playerStatisticsKeeper.find(player.botStatsId)
      supportResult <- playerStatisticsKeeper.find(player.supportStatsId)
    } yield (topResult, jungleResult, midResult, botResult, supportResult) match {
      case (Right(top), Right(jungle), Right(mid), Right(bot), Right(support)) =>
        Right(PlayerWithStatistics(player.discordId, player.discordUsername, player.gameUsername, top, jungle, mid, bot, support))
      case _ =>
        Left(StatisticsNotFound)
    }

  def resolveAll(players: Seq[PlayerKeeper.StoredPlayer]): Future[Either[DatabaseError, Seq[PlayerWithStatistics]]] =
    for {
      resolvedPlayers <- Future.sequence(players.map(foundPs => resolvePlayer(foundPs)))
      foldedPlayers = Utils.sequenceEither(resolvedPlayers)
    } yield foldedPlayers

  def find(discordId: UserId): Future[Either[DatabaseError, StoredPlayer]] = playerKeeper.find(discordId)

  def findAndResolve(discordId: UserId): Future[Either[DatabaseError, PlayerWithStatistics]] =
    for {
      player <- find(discordId)
      resolvedPlayer <- Utils.foldEitherOfFuture(player.map(resolvePlayer))
    } yield resolvedPlayer.flatten

  def findAndResolveAll(players: Seq[UserId]): Future[Either[DatabaseError, Seq[PlayerWithStatistics]]] =
    for {
      foundAllResult <- playerKeeper.findAll(players)
      resolvedAll <- Utils.foldEitherOfFuture(foundAllResult.map(resolveAll)).map(_.flatten)
    } yield resolvedAll

  def getAllPlayers: Future[Either[DatabaseError, Seq[PlayerWithStatistics]]] =
    for {
      foundAllResult <- playerKeeper.getAll
      resolvedPlayers <- resolveAll(foundAllResult)
    } yield resolvedPlayers

  def resetRating: Future[Int] = playerStatisticsKeeper.resetAll(RatingUtils.defaultGameInfo)
}
