package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.database.keepers.PlayerKeeper.StoredPlayer
import com.scustoms.services.MatchService.{MatchRole, OngoingMatch}
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

  case class QueuedPlayer(role: QueueRole, player: StoredPlayer) {
    def discordId: UserId = player.discordId
  }
}

class QueueService {

  private var queue: Seq[QueuedPlayer] = Seq.empty
  private var watchers: Seq[UserId] = Seq.empty

  def getQueue: Seq[QueuedPlayer] = queue

  def getWatchers: Seq[UserId] = watchers

  def upsertPlayer(player: QueuedPlayer): Boolean = {
    val res = this.remove(player.player.discordId)
    queue = queue.appended(player)
    res
  }

  def upsertWatcher(userId: UserId): Boolean = {
    val res = this.remove(userId)
    watchers = watchers.appended(userId)
    res
  }

  def remove(playerId: UserId): Boolean =
    if (this.contains(playerId)) {
      queue = queue.filterNot(_.player.discordId == playerId)
      watchers = watchers.filterNot(_ == playerId)
      true
    } else {
      false
    }

  def containsPlayer(playerId: UserId): Boolean = queue.exists(_.player.discordId == playerId)

  def containsWatcher(watcherId: UserId): Boolean = watchers.contains(watcherId)

  def contains(userId: UserId): Boolean = containsPlayer(userId) || containsWatcher(userId)

  def clearPlayers(): Unit = queue = Seq.empty

  def clearWatchers(): Unit = watchers = Seq.empty

  def clear(): Unit = {
    clearPlayers()
    clearWatchers()
  }

  def queueSize: Int = queue.length

  def watchersSize: Int = watchers.length

  def remaining(o: OngoingMatch): Seq[QueuedPlayer] = {
    val inGamePlayers = o.team1.seq ++ o.team2.seq
    getQueue.filterNot(p => inGamePlayers.exists(_.state.discordId == p.player.discordId))
  }
}
