package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.Utils
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.database.keepers.PlayerKeeper.{PlayerAlreadyExists, StoredPlayer}
import com.scustoms.database.keepers.PlayerStatisticsKeeper.PlayerStatistics
import com.scustoms.database.keepers.{PlayerKeeper, PlayerStatisticsKeeper}
import com.scustoms.services.MatchService.MatchRole
import com.scustoms.trueskill.RatingUtils
import de.gesundkrank.jskills.Rating

import scala.concurrent.{ExecutionContext, Future}

object PlayerService {
  case class PlayerWithStatistics(discordId: UserId, discordUsername: String, gameUsername: String,
                                  topStats: PlayerStatistics, jungleStats: PlayerStatistics, midStats: PlayerStatistics,
                                  botStats: PlayerStatistics, supportStats: PlayerStatistics) {
    def getBestStatistics: Option[(MatchRole, PlayerStatistics)] = {
      val stats = Seq(
        Option.when(topStats.games > 0)     ((topStats.rating.getConservativeRating,     (MatchService.Top, topStats))),
        Option.when(jungleStats.games > 0)  ((jungleStats.rating.getConservativeRating,  (MatchService.Jungle, jungleStats))),
        Option.when(midStats.games > 0)     ((midStats.rating.getConservativeRating,     (MatchService.Mid, midStats))),
        Option.when(botStats.games > 0)     ((botStats.rating.getConservativeRating,     (MatchService.Bot, botStats))),
        Option.when(supportStats.games > 0) ((supportStats.rating.getConservativeRating, (MatchService.Support, supportStats))),
      ).flatten
      Option.unless(stats.isEmpty) {
        stats.maxBy(_._1)._2
      }
    }

    def getRoleStatistics(role: MatchRole): PlayerStatistics = {
      role match {
        case MatchService.Top     => topStats
        case MatchService.Jungle  => jungleStats
        case MatchService.Mid     => midStats
        case MatchService.Bot     => botStats
        case MatchService.Support => supportStats
      }
    }

    def niceString(role: Option[MatchRole]): String = role match {
      case Some(r) => f"${getRoleStatistics(r).rating.getConservativeRating}%03.02f"
      case None    => ""
    }

    def totalGames: Long = topStats.games + jungleStats.games + midStats.games + botStats.games + supportStats.games

    def totalWins: Long = topStats.wins + jungleStats.wins + midStats.wins + botStats.wins + supportStats.wins

    def toStoredPlayer: StoredPlayer = StoredPlayer(0L, discordId, discordUsername, gameUsername,
      topStats.id, jungleStats.id, midStats.id, botStats.id, supportStats.id)

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

  def insert(discordId: UserId, discordUsername: String, gameUsername: String): Future[Either[DatabaseError, Long]] = {
    exists(discordId).flatMap {
      case false =>
        for {
          top <- playerStatisticsKeeper.insert(discordId)
          jungle <- playerStatisticsKeeper.insert(discordId)
          mid <- playerStatisticsKeeper.insert(discordId)
          bot <- playerStatisticsKeeper.insert(discordId)
          support <- playerStatisticsKeeper.insert(discordId)
          playerCreate = StoredPlayer(0L, discordId, discordUsername, gameUsername, top, jungle, mid, bot, support)
          insertResult <- playerKeeper.insert(playerCreate)
        } yield insertResult
      case true =>
        Future.successful(Left(PlayerAlreadyExists))
    }
  }

  def update(statistics: PlayerStatistics): Future[Int] = {
    playerStatisticsKeeper.update(statistics)
  }

  def updateAll(player: PlayerWithStatistics): Future[Int] = {
    update(player.topStats)
    update(player.jungleStats)
    update(player.midStats)
    update(player.botStats)
    update(player.supportStats)
    updateUsername(player)
  }

  def updateUsername(player: PlayerWithStatistics): Future[Int] = {
    playerKeeper.updateUsernames(player.discordId, player.discordUsername, player.discordUsername)
  }

  def exists(discordId: UserId): Future[Boolean] = playerKeeper.exists(discordId)

  def resolvePlayer(player: PlayerKeeper.StoredPlayer): Future[Option[PlayerWithStatistics]] = {
    for {
      topResult <- playerStatisticsKeeper.find(player.topStatsId)
      jungleResult <- playerStatisticsKeeper.find(player.jungleStatsId)
      midResult <- playerStatisticsKeeper.find(player.midStatsId)
      botResult <- playerStatisticsKeeper.find(player.botStatsId)
      supportResult <- playerStatisticsKeeper.find(player.supportStatsId)
    } yield (topResult, jungleResult, midResult, botResult, supportResult) match {
      case (Some(top), Some(jungle), Some(mid), Some(bot), Some(support)) =>
        Some(PlayerWithStatistics(player.discordId, player.discordUsername, player.gameUsername, top, jungle, mid, bot, support))
      case _ =>
        None
    }
  }

  def find(discordId: UserId): Future[Option[PlayerWithStatistics]] = {
    playerKeeper.find(discordId).flatMap {
      case Some(player) =>
        resolvePlayer(player)
      case None =>
        Future.successful(None)
    }
  }

  def findAll(players: Seq[UserId]): Future[Either[DatabaseError, Seq[PlayerWithStatistics]]] = {
    for {
      foundAllResult <- playerKeeper.findAll(players)
      resolvedPlayerOpt <- Utils.foldEitherOfFutureSeq(foundAllResult.map(foundPs => foundPs.map(resolvePlayer)))
    } yield resolvedPlayerOpt.map(_.flatten)
  }

  def getAllPlayers: Future[Seq[PlayerWithStatistics]] = {
    for {
      foundAllResult <- playerKeeper.getAll
      resolvedPlayerOpt <- Future.sequence(foundAllResult.map(resolvePlayer))
    } yield resolvedPlayerOpt.flatten
  }

  def resetRating: Future[Int] = playerStatisticsKeeper.resetAll(RatingUtils.defaultGameInfo)
}
