package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.Utils
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.database.keepers.PlayerKeeper.{PlayerAlreadyExists, PlayerCreate, PlayerWithStatistics}
import com.scustoms.database.keepers.PlayerStatisticsKeeper.PlayerStatistics
import com.scustoms.database.keepers.{PlayerKeeper, PlayerStatisticsKeeper}

import scala.concurrent.{ExecutionContext, Future}

class PlayerService(playerKeeper: PlayerKeeper, playerStatisticsKeeper: PlayerStatisticsKeeper)(implicit ec: ExecutionContext) {
  def insert(discordId: UserId, discordUsername: String, gameUsername: String): Future[Either[DatabaseError, Int]] = {
    for {
      playerExists <- playerKeeper.exists(discordId)
      _ = if (playerExists) Left(PlayerAlreadyExists) else Right(())
      top <- playerStatisticsKeeper.insert(discordId)
      jungle <- playerStatisticsKeeper.insert(discordId)
      mid <- playerStatisticsKeeper.insert(discordId)
      bot <- playerStatisticsKeeper.insert(discordId)
      support <- playerStatisticsKeeper.insert(discordId)
      playerCreate = PlayerCreate(discordId, discordUsername, gameUsername, top, jungle, mid, bot, support)
      insertResult <- playerKeeper.insert(playerCreate)
    } yield Right(insertResult)
  }

  def update(statistics: PlayerStatistics): Future[Int] = {
    playerStatisticsKeeper.update(statistics)
  }

  def updateAll(player: PlayerWithStatistics): Future[Int] = {
    playerStatisticsKeeper.update(player.top)
    playerStatisticsKeeper.update(player.jungle)
    playerStatisticsKeeper.update(player.mid)
    playerStatisticsKeeper.update(player.bot)
    playerStatisticsKeeper.update(player.support)
    updateUsername(player)
  }

  def updateUsername(player: PlayerWithStatistics): Future[Int] = {
    playerKeeper.updateUsernames(player.discordId, player.discordUsername, player.discordUsername)
  }

  def exists(discordId: UserId): Future[Boolean] = playerKeeper.exists(discordId)

  def resolvePlayer(player: PlayerKeeper.Player): Future[Option[PlayerWithStatistics]] = {
    for {
      topResult <- playerStatisticsKeeper.find(player.top)
      jungleResult <- playerStatisticsKeeper.find(player.jungle)
      midResult <- playerStatisticsKeeper.find(player.mid)
      botResult <- playerStatisticsKeeper.find(player.bot)
      supportResult <- playerStatisticsKeeper.find(player.support)
    } yield (topResult, jungleResult, midResult, botResult, supportResult) match {
      case (Some(top), Some(jungle), Some(mid), Some(bot), Some(support)) =>
        Some(PlayerWithStatistics(player.id, player.discordId, player.discordUsername, player.gameUsername, top, jungle, mid, bot, support))
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

  def resetRating: Future[Int] = playerStatisticsKeeper.resetAll(RatingService.defaultGameInfo)
}
