package com.scustoms.services

import com.scustoms.Utils
import com.scustoms.database.DatabaseManager.DatabaseError
import com.scustoms.database.keepers.MatchKeeper.StoredMatch
import com.scustoms.database.keepers.{MatchKeeper, PlayerKeeper}
import com.scustoms.services.MatchmakingService.{CompleteMatch, MatchTeam, OngoingMatch}

import scala.concurrent.{ExecutionContext, Future}

class MatchService(matchKeeper: MatchKeeper, playerKeeper: PlayerKeeper)(implicit ec: ExecutionContext) {

  def resolveMatches(seqStoredMatches: Seq[StoredMatch]): Future[Either[DatabaseError, Seq[CompleteMatch]]] = {
    val seqOfFutures = seqStoredMatches.map(m => {
      playerKeeper
        .findAll(m.teamA ++ m.teamB)
        .map {
          case Right(players) =>
            Right(CompleteMatch(m.team1Won, players.take(5), players.takeRight(5)))
          case Left(err) =>
            Left(err)
        }
    })
    val futureOfSeq = Future.sequence(seqOfFutures)
    futureOfSeq.map(Utils.sequenceEither)
  }

  def getLastN(n: Int): Future[Either[DatabaseError, Seq[CompleteMatch]]] = {
    matchKeeper
      .getLastN(n)
      .flatMap(resolveMatches)
  }

  def get(limit: Int, offset: Int): Future[Either[DatabaseError, Seq[CompleteMatch]]] = {
    matchKeeper
      .get(limit, offset)
      .flatMap(resolveMatches)
  }

  def insert(m: CompleteMatch): Future[Int] = {
    matchKeeper
      .insert(StoredMatch(0L, m.teamA.map(_.discordId), m.teamB.map(_.discordId), m.team1Won))
  }
}
