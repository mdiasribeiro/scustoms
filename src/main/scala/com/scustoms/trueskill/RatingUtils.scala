package com.scustoms.trueskill

import com.scustoms.services.MatchService._
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.MatchService
import de.gesundkrank.jskills.GameInfo

object RatingUtils {

  case class LaneMatchUp(quality: Double, player1: MatchPlayer, player2: MatchPlayer)

  val defaultGameInfo: GameInfo = GameInfo.getDefaultGameInfo

  def ratingFormat(r: Double): String = f"$r%03.02f"
  def percentageFormat(p: Double): String = f"$p%02.01f"

  def calculate(m: OngoingMatch, team1Won: Boolean): ResolvedMatch = {
    val (updatedTeam1, updatedTeam2) = TwoTeamCalculator.calculatePlayerRatings(defaultGameInfo, m.team1, m.team2, team1Won)
    ResolvedMatch(team1Won, updatedTeam1, updatedTeam2)
  }

  private def findLaneMatch(player1: MatchPlayer, others: Seq[MatchPlayer]): Option[LaneMatchUp] = {
    others.map { player2 =>
      val matchUp = TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, player1, player2)
      LaneMatchUp(matchUp, player1, player2)
    }.maxByOption(_.quality)
  }

  private def matchLane(players: Seq[MatchPlayer]): Option[LaneMatchUp] = {
    players.zipWithIndex.flatMap {
      case (player, index) =>
        findLaneMatch(player, players.drop(index + 1))
    }.maxByOption(_.quality)
  }

  def tryMatch(rolePlayers: Seq[MatchPlayer], fillPlayers: Seq[PlayerWithStatistics], role: MatchRole): Option[(LaneMatchUp, Seq[PlayerWithStatistics])] = {
    if (rolePlayers.length + fillPlayers.length >= 2) {
      val bestMatchUpOpt = rolePlayers.length match {
        case 0 =>
          val convertedFillPlayers = fillPlayers.map(_.toMatchPlayer(role))
          matchLane(convertedFillPlayers)
        case 1 =>
          val convertedFillPlayers = fillPlayers.map(_.toMatchPlayer(role))
          findLaneMatch(rolePlayers.head, convertedFillPlayers)
        case _ =>
          matchLane(rolePlayers)
      }

      bestMatchUpOpt.map(bestMatchUp => {
        val remainingPlayers = (rolePlayers.map(_.state) ++ fillPlayers).filterNot(p =>
          p.discordId == bestMatchUp.player1.state.discordId || p.discordId == bestMatchUp.player2.state.discordId)

        (bestMatchUp, remainingPlayers)
      })
    } else {
      None
    }
  }

  def arrangeTeams(top: LaneMatchUp, jungle: LaneMatchUp, mid: LaneMatchUp, bot: LaneMatchUp, support: LaneMatchUp): OngoingMatch = {
    val (t1, t2) = (top.player1, top.player2)

    val firstTeam = Seq(true, false)

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

  def reBalanceMatch(o: OngoingMatch): OngoingMatch = {
    val MatchTeam(t1, j1, m1, b1, s1) = o.team1
    val MatchTeam(t2, j2, m2, b2, s2) = o.team2
    arrangeTeams(
      LaneMatchUp(TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, t1, t2), t1, t2),
      LaneMatchUp(TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, j1, j2), j1, j2),
      LaneMatchUp(TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, m1, m2), m1, m2),
      LaneMatchUp(TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, b1, b2), b1, b2),
      LaneMatchUp(TwoPlayerCalculator.calculateLaneQuality(defaultGameInfo, s1, s2), s1, s2),
    )
  }

  def calculateRoleMatch(rolePlayers: Seq[MatchPlayer], fillQueuedPlayers: Seq[PlayerWithStatistics]): OngoingMatch = {
    require(rolePlayers.length + fillQueuedPlayers.length >= 10, NotEnoughPlayers)

    val playersByRole = rolePlayers.groupBy(_.role)

    val topPlayers = playersByRole.getOrElse(MatchService.Top, Seq.empty[MatchPlayer])
    val junglePlayers = playersByRole.getOrElse(MatchService.Jungle, Seq.empty[MatchPlayer])
    val midPlayers = playersByRole.getOrElse(MatchService.Mid, Seq.empty[MatchPlayer])
    val botPlayers = playersByRole.getOrElse(MatchService.Bot, Seq.empty[MatchPlayer])
    val supportPlayers = playersByRole.getOrElse(MatchService.Support, Seq.empty[MatchPlayer])
    var fillPlayers = fillQueuedPlayers

    var topMatchUp: Option[LaneMatchUp] = None
    var jungleMatchUp: Option[LaneMatchUp] = None
    var midMatchUp: Option[LaneMatchUp] = None
    var botMatchUp: Option[LaneMatchUp] = None
    var supportMatchUp: Option[LaneMatchUp] = None

    def matchRole(lanePairOpt: Option[LaneMatchUp], laneQueue: Seq[MatchPlayer], role: MatchRole): Option[LaneMatchUp] = {
      lanePairOpt.orElse {
        RatingUtils.tryMatch(laneQueue, fillPlayers, role).map {
          case (lanePair, remaining) =>
            fillPlayers = remaining
            lanePair
        }
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
