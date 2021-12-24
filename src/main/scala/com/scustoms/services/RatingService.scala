package com.scustoms.services

import com.scustoms.services.MatchmakingService.{CompleteMatch, MatchPlayer, MatchTeam, OngoingMatch}
import com.scustoms.services.QueueService.ExtendedQueuedPlayer
import com.scustoms.trueskill.{TwoPlayerCalculator, TwoTeamCalculator}
import de.gesundkrank.jskills.GameInfo

object RatingService {

  case class LaneMatchUp(quality: Double, player1: MatchPlayer, player2: MatchPlayer)

  val defaultGameInfo: GameInfo = GameInfo.getDefaultGameInfo

  def calculate(m: OngoingMatch, team1Won: Boolean): CompleteMatch = {
    val (updatedTeam1, updatedTeam2) = TwoTeamCalculator.calculatePlayerRatings(defaultGameInfo, m.team1, m.team2, team1Won)
    CompleteMatch(team1Won, updatedTeam1.seq.map(_.toPlayer), updatedTeam2.seq.map(_.toPlayer))
  }

  def tryMatch(players: Seq[ExtendedQueuedPlayer], role: QueueService.Role): Option[(LaneMatchUp, Seq[ExtendedQueuedPlayer])] = {
    val queueLength = players.length
    if (queueLength >= 2) {
      val results = (0 until queueLength).flatMap { i =>
        val player1 = MatchPlayer.fromExtendedQueuedPlayer(players(i), role)
        ((i + 1) until queueLength).map { j =>
          val player2 = MatchPlayer.fromExtendedQueuedPlayer(players(j), role)
          val matchUp = TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, player1, player2)
          LaneMatchUp(matchUp, player1, player2)
        }
      }
      val bestMatchUp = results.maxBy(_.quality)
      Some((bestMatchUp, players.filterNot(p => p.discordId == bestMatchUp.player1.discordId || p.discordId == bestMatchUp.player2.discordId)))
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
}
