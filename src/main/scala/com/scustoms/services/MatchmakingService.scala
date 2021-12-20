package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.database.keepers.PlayerKeeper.PlayerWithStatistics
import RatingService.LaneMatchUp
import com.scustoms.database.keepers.MatchKeeper
import com.scustoms.services.QueueService.{ExtendedQueuedPlayer, Role}

object MatchmakingService {
  object MatchPlayer {
    def fromExtendedQueuedPlayer(p: ExtendedQueuedPlayer, newRole: Role): MatchPlayer = MatchPlayer(p.discordId, newRole, p.playerWithStatistics)
  }
  case class MatchPlayer(discordId: UserId, role: Role, previousState: PlayerWithStatistics)

  case class Match(quality: Double, teamA: Seq[MatchPlayer], teamB: Seq[MatchPlayer])

  sealed trait MatchError
  case object NotEnoughPlayers extends MatchError
}

class MatchmakingService(matchKeeper: MatchKeeper) {
  import MatchmakingService._
  var ongoingMatch: Option[Match] = None

  def calculateRoleMatch(players: Seq[ExtendedQueuedPlayer]): Match = {
    require(players.length >= 10, NotEnoughPlayers)

    val playersByRole = players.groupBy(_.role)

    val topPlayers = playersByRole.getOrElse(QueueService.Top, Seq.empty[ExtendedQueuedPlayer])
    val junglePlayers = playersByRole.getOrElse(QueueService.Jungle, Seq.empty[ExtendedQueuedPlayer])
    val midPlayers = playersByRole.getOrElse(QueueService.Mid, Seq.empty[ExtendedQueuedPlayer])
    val botPlayers = playersByRole.getOrElse(QueueService.Bot, Seq.empty[ExtendedQueuedPlayer])
    val supportPlayers = playersByRole.getOrElse(QueueService.Support, Seq.empty[ExtendedQueuedPlayer])
    var fillPlayers = playersByRole.getOrElse(QueueService.Fill, Seq.empty[ExtendedQueuedPlayer])

    var tops: Option[LaneMatchUp] = None
    var jungles: Option[LaneMatchUp] = None
    var mids: Option[LaneMatchUp] = None
    var bots: Option[LaneMatchUp] = None
    var supports: Option[LaneMatchUp] = None

    def matchRole(lanePairOpt: Option[LaneMatchUp], laneQueue: Seq[ExtendedQueuedPlayer], role: Role): Option[LaneMatchUp] = {
      if (lanePairOpt.isEmpty) {
        RatingService.tryMatch(laneQueue, role) match {
          case Some((lanePair, remaining)) =>
            fillPlayers ++= remaining
            Some(lanePair)
          case None =>
            RatingService.tryMatch(laneQueue ++ fillPlayers, role).map {
              case (lanePair, remaining) =>
                fillPlayers = remaining
                lanePair
            }
        }
      } else {
        lanePairOpt
      }
    }

    val MAX_ITERATIONS = 10
    var i = 1
    while (tops.isEmpty || jungles.isEmpty || mids.isEmpty || bots.isEmpty || supports.isEmpty || i > MAX_ITERATIONS) {
      tops = matchRole(tops, topPlayers, QueueService.Top)
      jungles = matchRole(jungles, junglePlayers, QueueService.Jungle)
      mids = matchRole(mids, midPlayers, QueueService.Mid)
      bots = matchRole(bots, botPlayers, QueueService.Bot)
      supports = matchRole(supports, supportPlayers, QueueService.Support)
      i += 1
    }

    (tops, jungles, mids, bots, supports) match {
      case (Some(top), Some(jungle), Some(mid), Some(bot), Some(support)) =>
        RatingService.arrangeTeams(top, jungle, mid, bot, support)
      case other =>
        throw new Exception(s"Something went wrong with the match algorithm: $other")
    }
  }
}
