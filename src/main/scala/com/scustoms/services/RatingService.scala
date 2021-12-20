package com.scustoms.services

import com.scustoms.services.MatchmakingService.{Match, MatchPlayer}
import com.scustoms.services.QueueService.ExtendedQueuedPlayer
import de.gesundkrank.jskills.trueskill.{TwoPlayerTrueSkillCalculator, TwoTeamTrueSkillCalculator}
import de.gesundkrank.jskills.{GameInfo, IPlayer, ITeam, Rating}

import java.util

object RatingService {
  import scala.jdk.CollectionConverters._
  class ScustomsTeam extends util.HashMap[IPlayer, Rating] with ITeam

  case class LaneMatchUp(quality: Double, player1: MatchPlayer, player2: MatchPlayer)

  private val trueSkillTeamCalculator = new TwoTeamTrueSkillCalculator()
  private val trueSkillPlayerCalculator = new TwoPlayerTrueSkillCalculator()
  val gameInfo: GameInfo = GameInfo.getDefaultGameInfo

  def tryMatch(players: Seq[ExtendedQueuedPlayer], role: QueueService.Role): Option[(LaneMatchUp, Seq[ExtendedQueuedPlayer])] = {
    val team1: ITeam = new ScustomsTeam()
    val team2: ITeam = new ScustomsTeam()
    val queueLength = players.length
    if (queueLength >= 2) {
      val results = (0 until queueLength).flatMap { i =>
        val player1 = MatchPlayer.fromExtendedQueuedPlayer(players(i), role)
        swapTeamPlayers(team1, player1)
        ((i + 1) until queueLength).map { j =>
          val player2 = MatchPlayer.fromExtendedQueuedPlayer(players(j), role)
          swapTeamPlayers(team2, player2)
          val matchUp = trueSkillPlayerCalculator.calculateMatchQuality(gameInfo, Seq(team1, team2).asJava)
          LaneMatchUp(matchUp, player1, player2)
        }
      }
      val bestMatchUp = results.maxBy(_.quality)
      Some((bestMatchUp, players.filterNot(p => p.discordId == bestMatchUp.player1.discordId || p.discordId == bestMatchUp.player2.discordId)))
    } else {
      None
    }
  }

  def arrangeTeams(top: LaneMatchUp, jungle: LaneMatchUp, mid: LaneMatchUp, bot: LaneMatchUp, support: LaneMatchUp): Match = {
    val team1: ITeam = new ScustomsTeam()
    val team2: ITeam = new ScustomsTeam()
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
      swapTeamPlayers(team1, t1, j1, m1, b1, s1)
      swapTeamPlayers(team2, t2, j2, m2, b2, s2)

      val quality = trueSkillTeamCalculator.calculateMatchQuality(gameInfo, Seq(team1, team2).asJava)
      Match(quality, Seq(t1, j1, m1, b1, s1), Seq(t2, j2, m2, b2, s2))
    }

    allMatches.maxBy(_.quality)
  }

  def swapTeamPlayers(team: ITeam, players: MatchPlayer*): Unit = {
    team.clear()
    players.foreach(p => team.put(p.previousState, p.previousState.getRoleStatistics(p.role).get.rating))
  }
}
