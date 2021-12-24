package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.database.keepers.PlayerKeeper.{Player, PlayerWithStatistics}
import RatingService.LaneMatchUp
import com.scustoms.database.keepers.{MatchKeeper, PlayerKeeper}
import com.scustoms.services.QueueService.{ExtendedQueuedPlayer, Role}
import de.gesundkrank.jskills.Rating

object MatchmakingService {
  object MatchPlayer {
    def fromExtendedQueuedPlayer(p: ExtendedQueuedPlayer, newRole: Role): MatchPlayer = MatchPlayer(p.discordId, newRole, p.playerWithStatistics)
    def fromPlayerWithStatistics(p: PlayerWithStatistics, role: Role): MatchPlayer = MatchPlayer(p.discordId, role, p)
  }

  case class MatchPlayer(discordId: UserId, role: Role, state: PlayerWithStatistics) {
    def toPlayer: Player = state.toPlayer

    def getMatchRating: Rating = state.getRoleStatistics(role).rating
    def updatedRating(newRating: Rating): MatchPlayer = this.copy(state = state.updatedRating(role, newRating))
  }

  case class OngoingMatch(quality: Double, team1: MatchTeam, team2: MatchTeam)

  object MatchTeam {
    def fromPlayersWithStatistics(seq: Seq[PlayerWithStatistics]): MatchTeam = {
      val Seq(t, j, m, b, s) = seq
      MatchTeam(
        MatchPlayer.fromPlayerWithStatistics(t, QueueService.Top),
        MatchPlayer.fromPlayerWithStatistics(j, QueueService.Jungle),
        MatchPlayer.fromPlayerWithStatistics(m, QueueService.Mid),
        MatchPlayer.fromPlayerWithStatistics(b, QueueService.Bot),
        MatchPlayer.fromPlayerWithStatistics(s, QueueService.Support)
      )
    }
  }
  case class MatchTeam(top: MatchPlayer, jungle: MatchPlayer, mid: MatchPlayer, bot: MatchPlayer, support: MatchPlayer) {
    val seq: Seq[MatchPlayer] = Seq(top, jungle, mid, bot, support)

    def ratings: Seq[Rating] = seq.map(p => p.state.getRoleStatistics(p.role).rating)

    def updatedRatings(ratings: Seq[Rating]): MatchTeam = {
      require(ratings.length == 5, "Ratings sequence must be of length 5")

      val Seq(topRating, jungleRating, midRating, botRating, supportRating) = ratings
      MatchTeam(
        top.updatedRating(topRating),
        jungle.updatedRating(jungleRating),
        mid.updatedRating(midRating),
        bot.updatedRating(botRating),
        support.updatedRating(supportRating)
      )
    }
  }

  case class CompleteMatch(team1Won: Boolean, teamA: Seq[Player], teamB: Seq[Player])

  sealed trait MatchError
  case object NotEnoughPlayers extends MatchError
}

class MatchmakingService(playerKeeper: PlayerKeeper) {
  import MatchmakingService._
  var ongoingMatch: Option[OngoingMatch] = None

  def calculateRoleMatch(players: Seq[ExtendedQueuedPlayer]): OngoingMatch = {
    require(players.length >= 10, NotEnoughPlayers)

    val playersByRole = players.groupBy(_.role)

    val topPlayers = playersByRole.getOrElse(Some(QueueService.Top), Seq.empty[ExtendedQueuedPlayer])
    val junglePlayers = playersByRole.getOrElse(Some(QueueService.Jungle), Seq.empty[ExtendedQueuedPlayer])
    val midPlayers = playersByRole.getOrElse(Some(QueueService.Mid), Seq.empty[ExtendedQueuedPlayer])
    val botPlayers = playersByRole.getOrElse(Some(QueueService.Bot), Seq.empty[ExtendedQueuedPlayer])
    val supportPlayers = playersByRole.getOrElse(Some(QueueService.Support), Seq.empty[ExtendedQueuedPlayer])
    var fillPlayers = playersByRole.getOrElse(None, Seq.empty[ExtendedQueuedPlayer])

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
