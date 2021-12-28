package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.services.MatchService.MatchRole
import com.scustoms.services.PlayerService.PlayerWithStatistics
import com.scustoms.services.QueueService.QueuedPlayer

object QueueService {
  sealed trait QueueError {
    def message: String
  }
  final case object ErrorParsingRole extends QueueError {
    override def message: String = "Error parsing desired role."
  }
  final case object PlayerDoesNotExist extends QueueError {
    override def message: String = "Player was not found. Make sure you register before joining the queue."
  }
  final case object PlayerAlreadyInQueue extends QueueError {
    override def message: String = "Player is already in the queue."
  }
  final case object PlayerAlreadyInMatch extends QueueError {
    override def message: String = "Player is already in a match."
  }

  sealed trait QueueRole {
    def toMatchRole: Option[MatchRole]
  }
  final case object Top extends QueueRole { override def toMatchRole: Option[MatchRole] = Some(MatchService.Top) }
  final case object Jungle extends QueueRole { override def toMatchRole: Option[MatchRole] = Some(MatchService.Jungle) }
  final case object Mid extends QueueRole { override def toMatchRole: Option[MatchRole] = Some(MatchService.Mid) }
  final case object Bot extends QueueRole { override def toMatchRole: Option[MatchRole] = Some(MatchService.Bot) }
  final case object Support extends QueueRole { override def toMatchRole: Option[MatchRole] = Some(MatchService.Support) }
  final case object Fill extends QueueRole { override def toMatchRole: Option[MatchRole] = None }

  def parseRole(role: String): Option[QueueRole] = role.toLowerCase match {
    case "top"               => Some(Top)
    case "jungle"            => Some(Jungle)
    case "mid"               => Some(Mid)
    case "bot"               => Some(Bot)
    case "sup" | "support"   => Some(Support)
    case "fill" | "any"      => Some(Fill)
    case _                   => None
  }

  case class QueuedPlayer(role: QueueRole, stats: PlayerWithStatistics) {
    def discordId: UserId = stats.discordId
  }
}

class QueueService {

  private var queue: Seq[QueuedPlayer] = Seq.empty
  private var watchers: Seq[UserId] = Seq.empty

  def getQueue: Seq[QueuedPlayer] = queue

  def getWatchers: Seq[UserId] = watchers

  def addPlayer(player: QueuedPlayer): Boolean =
    if (this.contains(player.stats.discordId)) {
      false
    } else {
      queue = queue.appended(player)
      true
    }

  def addWatcher(userId: UserId): Boolean =
    if (this.contains(userId)) {
      false
    } else {
      watchers = watchers.appended(userId)
      true
    }

  def remove(playerId: UserId): Boolean =
    if (this.contains(playerId)) {
      queue = queue.filterNot(_.stats.discordId == playerId)
      watchers = watchers.filterNot(_ == playerId)
      true
    } else {
      false
    }

  def containsPlayer(playerId: UserId): Boolean = queue.exists(_.stats.discordId == playerId)

  def containsWatcher(watcherId: UserId): Boolean = watchers.contains(watcherId)

  def contains(userId: UserId): Boolean = containsPlayer(userId) || containsWatcher(userId)

  def clearPlayers(): Unit = queue = Seq.empty

  def clearWatchers(): Unit = watchers = Seq.empty

  def clear(): Unit = {
    clearPlayers()
    clearWatchers()
  }

  def length: Int = queue.length
}
