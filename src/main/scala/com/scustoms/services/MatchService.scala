package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.database.keepers.MatchKeeper.{StoredMatch, StoredMatchTeam}
import com.scustoms.database.keepers.PlayerKeeper.StoredPlayer
import com.scustoms.database.keepers.MatchKeeper
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.QueueService.QueuedPlayer
import com.scustoms.trueskill.{RatingUtils, TwoTeamCalculator}
import de.gesundkrank.jskills.Rating

import scala.concurrent.{ExecutionContext, Future}

object MatchService {
  sealed trait MatchRole {
    def toQueueRole: QueueService.QueueRole
  }
  final case object Top extends MatchRole { override def toQueueRole: QueueService.QueueRole = QueueService.Top }
  final case object Jungle extends MatchRole { override def toQueueRole: QueueService.QueueRole = QueueService.Jungle }
  final case object Mid extends MatchRole { override def toQueueRole: QueueService.QueueRole = QueueService.Mid }
  final case object Bot extends MatchRole { override def toQueueRole: QueueService.QueueRole = QueueService.Bot }
  final case object Support extends MatchRole { override def toQueueRole: QueueService.QueueRole = QueueService.Support }

  def matchRoleFromQueueRole(role: QueueService.QueueRole): Option[MatchRole] = role match {
    case QueueService.Top => Some(Top)
    case QueueService.Jungle => Some(Jungle)
    case QueueService.Mid => Some(Mid)
    case QueueService.Bot => Some(Bot)
    case QueueService.Support => Some(Support)
    case QueueService.Fill => None
  }

  object MatchPlayer {
    def fromQueuedPlayer(p: QueuedPlayer, newRole: MatchRole): MatchPlayer = MatchPlayer(newRole, p.stats)
    def fromPlayerWithStatistics(p: PlayerWithStatistics, role: MatchRole): MatchPlayer = MatchPlayer(role, p)
  }

  case class MatchPlayer(role: MatchRole, state: PlayerWithStatistics) {
    def toPlayer: StoredPlayer = state.toStoredPlayer

    def getMatchRating: Rating = state.getRoleStatistics(role).rating
    def updatedRating(newRating: Rating, won: Boolean): MatchPlayer =
      this.copy(state = state.updatedRating(role, newRating, won))
  }

  object OngoingMatch {
    def fromResolvedMatch(m: ResolvedMatch): OngoingMatch = {
      val score = TwoTeamCalculator.calculateMatchQuality(RatingUtils.defaultGameInfo, m.teamA, m.teamB)
      OngoingMatch(score, m.teamA, m.teamB)
    }
  }
  case class OngoingMatch(quality: Double, team1: MatchTeam, team2: MatchTeam)

  object MatchTeam {
    def fromPlayersWithStatistics(seq: Seq[PlayerWithStatistics]): MatchTeam = {
      val Seq(t, j, m, b, s) = seq
      MatchTeam(
        MatchPlayer.fromPlayerWithStatistics(t, MatchService.Top),
        MatchPlayer.fromPlayerWithStatistics(j, MatchService.Jungle),
        MatchPlayer.fromPlayerWithStatistics(m, MatchService.Mid),
        MatchPlayer.fromPlayerWithStatistics(b, MatchService.Bot),
        MatchPlayer.fromPlayerWithStatistics(s, MatchService.Support)
      )
    }
  }
  case class MatchTeam(top: MatchPlayer, jungle: MatchPlayer, mid: MatchPlayer, bot: MatchPlayer, support: MatchPlayer) {
    val seq: Seq[MatchPlayer] = Seq(top, jungle, mid, bot, support)

    def ratings: Seq[Rating] = seq.map(p => p.state.getRoleStatistics(p.role).rating)

    def toStoredMatchTeam: StoredMatchTeam = StoredMatchTeam(
      top.state.discordId, jungle.state.discordId, mid.state.discordId, bot.state.discordId, support.state.discordId
    )

    def updatedRatings(ratings: Seq[Rating], won: Boolean): MatchTeam = {
      require(ratings.length == 5, "Ratings sequence must be of length 5")

      val Seq(topRating, jungleRating, midRating, botRating, supportRating) = ratings
      MatchTeam(
        top.updatedRating(topRating, won),
        jungle.updatedRating(jungleRating, won),
        mid.updatedRating(midRating, won),
        bot.updatedRating(botRating, won),
        support.updatedRating(supportRating, won)
      )
    }
  }

  case class ResolvedMatch(team1Won: Boolean, teamA: MatchTeam, teamB: MatchTeam)

  sealed trait MatchError
  case object NotEnoughPlayers extends MatchError
}

class MatchService(matchKeeper: MatchKeeper, playerService: PlayerService)(implicit ec: ExecutionContext) {
  import MatchService._

  var ongoingMatch: Option[OngoingMatch] = None

  def contains(userId: UserId): Boolean = ongoingMatch.exists(m =>
    m.team1.seq.exists(_.state.discordId == userId) || m.team2.seq.exists(_.state.discordId == userId))

  def resolveMatch(m: StoredMatch): Future[Option[ResolvedMatch]] = {
    for {
      teamAResult <- resolveTeam(m.teamA)
      teamBResult <- resolveTeam(m.teamB)
    } yield (teamAResult, teamBResult) match {
      case (Some(teamA), Some(teamB)) =>
        Some(ResolvedMatch(m.team1Won, teamA, teamB))
      case _ =>
        None
    }
  }

  def resolveMatches(seqStoredMatches: Seq[StoredMatch]): Future[Seq[Option[ResolvedMatch]]] = {
    val seqOfFutures = seqStoredMatches.map(m => resolveMatch(m))
    Future.sequence(seqOfFutures)
  }

  def getLastN(n: Int): Future[Seq[ResolvedMatch]] =
    matchKeeper
      .getLastN(n)
      .flatMap(resolveMatches)
      .map(_.flatten)

  def get(limit: Int, offset: Int): Future[Seq[ResolvedMatch]] =
    getUnresolved(limit, offset)
      .flatMap(resolveMatches)
      .map(_.flatten)

  def getUnresolved(limit: Int, offset: Int): Future[Seq[StoredMatch]] =
    matchKeeper
      .get(limit, offset)

  def insert(m: ResolvedMatch): Future[Int] =
    matchKeeper
      .insert(StoredMatch(0L, m.teamA.toStoredMatchTeam, m.teamB.toStoredMatchTeam, m.team1Won))

  def updateTeamRatings(m: MatchTeam): Future[Unit] =
    for {
      _ <- playerService.update(m.top.state.topStats)
      _ <- playerService.update(m.jungle.state.jungleStats)
      _ <- playerService.update(m.mid.state.midStats)
      _ <- playerService.update(m.bot.state.botStats)
      _ <- playerService.update(m.support.state.supportStats)
    } yield ()

  def insertAndUpdate(resolvedMatch: ResolvedMatch): Future[Unit] = {
    for {
      _ <- insert(resolvedMatch)
      _ <- updateRatings(resolvedMatch)
    } yield ()
  }

  def updateRatings(resolvedMatch: ResolvedMatch): Future[Unit] = {
    for {
      _ <- updateTeamRatings(resolvedMatch.teamA)
      _ <- updateTeamRatings(resolvedMatch.teamB)
    } yield ()
  }

  def resolveTeam(team: StoredMatchTeam): Future[Option[MatchTeam]] =
    for {
      topResult <- playerService.find(team.topId)
      jungleResult <- playerService.find(team.jungleId)
      midResult <- playerService.find(team.midId)
      botResult <- playerService.find(team.botId)
      supportResult <- playerService.find(team.supportId)
    } yield (topResult, jungleResult, midResult, botResult, supportResult) match {
      case (Some(top), Some(jungle), Some(mid), Some(bot), Some(support)) =>
        Some(MatchTeam(
          MatchPlayer(MatchService.Top, top),
          MatchPlayer(MatchService.Jungle, jungle),
          MatchPlayer(MatchService.Mid, mid),
          MatchPlayer(MatchService.Bot, bot),
          MatchPlayer(MatchService.Support, support),
        ))
      case _ =>
        None
    }
}
