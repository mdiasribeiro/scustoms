package com.scustoms.trueskill

import com.scustoms.services.MatchService._
import com.scustoms.services.{MatchService, QueueService}
import com.scustoms.services.QueueService.QueuedPlayer
import de.gesundkrank.jskills.GameInfo

object RatingUtils {

  case class LaneMatchUp(quality: Double, player1: MatchPlayer, player2: MatchPlayer)

  val defaultGameInfo: GameInfo = GameInfo.getDefaultGameInfo

  def calculate(m: OngoingMatch, team1Won: Boolean): ResolvedMatch = {
    val (updatedTeam1, updatedTeam2) = TwoTeamCalculator.calculatePlayerRatings(defaultGameInfo, m.team1, m.team2, team1Won)
    ResolvedMatch(team1Won, updatedTeam1, updatedTeam2)
  }

  def tryMatch(players: Seq[QueuedPlayer], role: MatchRole): Option[(LaneMatchUp, Seq[QueuedPlayer])] = {
    val queueLength = players.length
    if (queueLength >= 2) {
      val results = (0 until queueLength).flatMap { i =>
        val player1 = MatchPlayer.fromQueuedPlayer(players(i), role)
        ((i + 1) until queueLength).map { j =>
          val player2 = MatchPlayer.fromQueuedPlayer(players(j), role)
          val matchUp = TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, player1, player2)
          LaneMatchUp(matchUp, player1, player2)
        }
      }
      val bestMatchUp = results.maxBy(_.quality)
      val remainingPlayers = players.filterNot(p =>
        p.stats.discordId == bestMatchUp.player1.state.discordId || p.stats.discordId == bestMatchUp.player2.state.discordId)

      Some((bestMatchUp, remainingPlayers))
    } else {
      None
    }
  }

  def arrangeTeams(top: LaneMatchUp, jungle: LaneMatchUp, mid: LaneMatchUp, bot: LaneMatchUp, support: LaneMatchUp): OngoingMatch = {
    val (t1, t2) = (top.player1, top.player2)

    val firstTeam = Seq(false, true)

    val allMatches = for {
      j <- firstTeam
      m <- firstTeam
      b <- firstTeam
      s <- firstTeam
      (j1, j2) = if (j) (jungle.player1, jungle.player2) else (jungle.player2, jungle.player1)
      (m1, m2) = if (m) (mid.player1, mid.player2) else (mid.player2, mid.player1)
      (b1, b2) = if (b) (bot.player1, bot.player2) else (bot.player2, bot.player1)
      (s1, s2) = if (s) (support.player1, support.player2) else (support.player2, support.player1)
    } yield {
      val team1 = MatchTeam(t1, j1, m1, b1, s1)
      val team2 = MatchTeam(t2, j2, m2, b2, s2)

      val quality = TwoTeamCalculator.calculateMatchQuality(defaultGameInfo, team1, team2)
      OngoingMatch(quality, team1, team2)
    }

    allMatches.maxBy(_.quality)
  }

  def calculateRoleMatch(players: Seq[QueuedPlayer]): OngoingMatch = {
    require(players.length >= 10, NotEnoughPlayers)

    val playersByRole = players.groupBy(_.role)

    val topPlayers = playersByRole.getOrElse(QueueService.Top, Seq.empty[QueuedPlayer])
    val junglePlayers = playersByRole.getOrElse(QueueService.Jungle, Seq.empty[QueuedPlayer])
    val midPlayers = playersByRole.getOrElse(QueueService.Mid, Seq.empty[QueuedPlayer])
    val botPlayers = playersByRole.getOrElse(QueueService.Bot, Seq.empty[QueuedPlayer])
    val supportPlayers = playersByRole.getOrElse(QueueService.Support, Seq.empty[QueuedPlayer])
    var fillPlayers = playersByRole.getOrElse(QueueService.Fill, Seq.empty[QueuedPlayer])

    var topMatchUp: Option[LaneMatchUp] = None
    var jungleMatchUp: Option[LaneMatchUp] = None
    var midMatchUp: Option[LaneMatchUp] = None
    var botMatchUp: Option[LaneMatchUp] = None
    var supportMatchUp: Option[LaneMatchUp] = None

    def matchRole(lanePairOpt: Option[LaneMatchUp], laneQueue: Seq[QueuedPlayer], role: MatchRole): Option[LaneMatchUp] = {
      if (lanePairOpt.isEmpty) {
        RatingUtils.tryMatch(laneQueue, role) match {
          case Some((lanePair, remaining)) =>
            fillPlayers ++= remaining
            Some(lanePair)
          case None =>
            RatingUtils.tryMatch(laneQueue ++ fillPlayers, role).map {
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
    while (topMatchUp.isEmpty || jungleMatchUp.isEmpty || midMatchUp.isEmpty || botMatchUp.isEmpty || supportMatchUp.isEmpty || i > MAX_ITERATIONS) {
      topMatchUp = matchRole(topMatchUp, topPlayers, MatchService.Top)
      jungleMatchUp = matchRole(jungleMatchUp, junglePlayers, MatchService.Jungle)
      midMatchUp = matchRole(midMatchUp, midPlayers, MatchService.Mid)
      botMatchUp = matchRole(botMatchUp, botPlayers, MatchService.Bot)
      supportMatchUp = matchRole(supportMatchUp, supportPlayers, MatchService.Support)
      i += 1
    }

    (topMatchUp, jungleMatchUp, midMatchUp, botMatchUp, supportMatchUp) match {
      case (Some(top), Some(jungle), Some(mid), Some(bot), Some(support)) =>
        RatingUtils.arrangeTeams(top, jungle, mid, bot, support)
      case other =>
        throw new Exception(s"Something went wrong with the match algorithm: $other")
    }
  }
}
