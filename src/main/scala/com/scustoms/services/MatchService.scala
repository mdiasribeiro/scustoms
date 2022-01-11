package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.database.keepers.MatchKeeper.{StoredMatch, StoredMatchTeam}
import com.scustoms.database.keepers.PlayerKeeper.StoredPlayer
import com.scustoms.database.keepers.MatchKeeper
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.QueueService.{QueueRole, QueuedPlayer}
import com.scustoms.trueskill.RatingUtils.defaultGameInfo
import com.scustoms.trueskill.{RatingUtils, TwoTeamCalculator}
import de.gesundkrank.jskills.Rating

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

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

  case class MatchPlayer(role: MatchRole, state: PlayerWithStatistics) {
    def toPlayer: StoredPlayer = state.toStoredPlayer

    def getMatchRating: Rating = state.getRoleStatistics(role).rating

    def updatedRating(newRating: Rating, won: Boolean): MatchPlayer =
      this.copy(state = state.updatedRating(role, newRating, won))

    def toQueuedPlayer(role: QueueRole): QueuedPlayer = QueuedPlayer(role, state)
  }

  object OngoingMatch {
    def fromResolvedStoredMatch(m: ResolvedStoredMatch): OngoingMatch = {
      val score = TwoTeamCalculator.calculateMatchQuality(RatingUtils.defaultGameInfo, m.teamA, m.teamB)
      OngoingMatch(score, m.teamA, m.teamB)
    }

    def fromTeams(teamA: MatchTeam, teamB: MatchTeam): OngoingMatch = {
      val matchQuality = TwoTeamCalculator.calculateMatchQuality(defaultGameInfo, teamA, teamB)
      OngoingMatch(matchQuality, teamA, teamB)
    }
  }
  case class OngoingMatch(quality: Double, team1: MatchTeam, team2: MatchTeam) {
    def contains(userId: UserId): Boolean = {
      team1.seq.exists(_.state.discordId == userId) || team2.seq.exists(_.state.discordId == userId)
    }

    def getTeam(teamA: Boolean): MatchTeam = if (teamA) team1 else team2

    def find(userId: UserId): Option[(MatchPlayer, Boolean)] =
      team1.find(userId).map(p => (p, true)).orElse(team2.find(userId).map(p => (p, false)))
  }

  object MatchTeam {
    def fromPlayersWithStatistics(seq: Seq[PlayerWithStatistics]): MatchTeam = {
      val Seq(t, j, m, b, s) = seq
      MatchTeam(
        t.toMatchPlayer(MatchService.Top),
        j.toMatchPlayer(MatchService.Jungle),
        m.toMatchPlayer(MatchService.Mid),
        b.toMatchPlayer(MatchService.Bot),
        s.toMatchPlayer(MatchService.Support)
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

    def find(userId: UserId): Option[MatchPlayer] = seq.find(_.state.discordId == userId)

    def swapPlayer(role: MatchRole, newPlayer: PlayerWithStatistics): MatchTeam = {
      role match {
        case Top => this.copy(top = MatchPlayer(role, newPlayer))
        case Jungle => this.copy(jungle = MatchPlayer(role, newPlayer))
        case Mid => this.copy(mid = MatchPlayer(role, newPlayer))
        case Bot => this.copy(bot = MatchPlayer(role, newPlayer))
        case Support => this.copy(support = MatchPlayer(role, newPlayer))
      }
    }
  }

  case class ResolvedMatch(team1Won: Boolean, teamA: MatchTeam, teamB: MatchTeam)

  case class ResolvedStoredMatch(id: Long, team1Won: Boolean, teamA: MatchTeam, teamB: MatchTeam) {
    def toResolvedMatch: ResolvedMatch = ResolvedMatch(team1Won, teamA, teamB)
  }

  sealed trait MatchError
  case object NotEnoughPlayers extends MatchError
}

class MatchService(matchKeeper: MatchKeeper, playerService: PlayerService)(implicit ec: ExecutionContext) {
  import MatchService._

  var ongoingMatch: Option[OngoingMatch] = None

  def clearMatch(): Boolean = {
    val existed = ongoingMatch.isDefined
    ongoingMatch = None
    existed
  }

  def swapPlayers(userId1: UserId, userId2: UserId): Option[OngoingMatch] = {
    ongoingMatch.flatMap(m => {
      (m.find(userId1), m.find(userId2)) match {
        case (Some((p1, p1team)), Some((p2, p2team))) =>
          val (teamA, teamB) = if (p1team == p2team) {
            val teamA = m.getTeam(p1team).swapPlayer(p1.role, p2.state).swapPlayer(p2.role, p1.state)
            val teamB = m.getTeam(!p1team)
            (teamA, teamB)
          } else {
            val teamA = m.getTeam(p1team).swapPlayer(p1.role, p2.state)
            val teamB = m.getTeam(p2team).swapPlayer(p2.role, p1.state)
            (teamA, teamB)
          }
          if (p1team) Some(OngoingMatch.fromTeams(teamA, teamB)) else Some(OngoingMatch.fromTeams(teamB, teamA))
        case _ =>
          None
      }
    })
  }

  def swapPlayer(userId1: UserId, otherPlayer: PlayerWithStatistics): Option[OngoingMatch] = {
    ongoingMatch.flatMap(m => {
      m.find(userId1) match {
        case Some((p1, p1team)) =>
          val teamA = m.getTeam(p1team).swapPlayer(p1.role, otherPlayer)
          val teamB = m.getTeam(!p1team)
          if (p1team) Some(OngoingMatch.fromTeams(teamA, teamB)) else Some(OngoingMatch.fromTeams(teamB, teamA))
        case _ =>
          None
      }
    })
  }

  def contains(userId: UserId): Boolean = ongoingMatch.exists(m =>
    m.team1.seq.exists(_.state.discordId == userId) || m.team2.seq.exists(_.state.discordId == userId))

  def resolveStoredMatch(m: StoredMatch): Future[Option[ResolvedStoredMatch]] = {
    for {
      teamAResult <- resolveTeam(m.teamA)
      teamBResult <- resolveTeam(m.teamB)
    } yield (teamAResult, teamBResult) match {
      case (Some(teamA), Some(teamB)) =>
        Some(ResolvedStoredMatch(m.id, m.team1Won, teamA, teamB))
      case _ =>
        None
    }
  }

  def resolveStoredMatches(seqStoredMatches: Seq[StoredMatch]): Future[Seq[Option[ResolvedStoredMatch]]] = {
    val seqOfFutures = seqStoredMatches.map(m => resolveStoredMatch(m))
    Future.sequence(seqOfFutures)
  }

  def getLastN(n: Int): Future[Seq[ResolvedStoredMatch]] =
    matchKeeper
      .getLastN(n)
      .flatMap(resolveStoredMatches)
      .map(_.flatten)

  def get(limit: Int, offset: Int): Future[Seq[ResolvedStoredMatch]] =
    getUnresolved(limit, offset)
      .flatMap(resolveStoredMatches)
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

  def updateRatingsSync(resolvedMatch: ResolvedMatch): Unit = {
    val f = for {
      _ <- updateTeamRatings(resolvedMatch.teamA)
      _ <- updateTeamRatings(resolvedMatch.teamB)
    } yield ()
    Await.result(f, 10.seconds)
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

  def changeResult(id: Long, team1Won: Boolean): Future[Int] = matchKeeper.changeResult(id, team1Won)
}
