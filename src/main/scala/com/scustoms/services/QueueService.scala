package com.scustoms.services

import ackcord.data.UserId
import com.scustoms.services.MatchService.{MatchPlayer, MatchRole, OngoingMatch}
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
    case "top"                        => Some(Top)
    case "jun" | "jung" | "jungle"    => Some(Jungle)
    case "mid"                        => Some(Mid)
    case "bot" | "adc"                => Some(Bot)
    case "sup" | "supp" | "support"   => Some(Support)
    case "fill" | "any"               => Some(Fill)
    case _                            => None
  }

  case class QueuedPlayer(role: QueueRole, stats: PlayerWithStatistics) {
    def toMatchPlayer(role: MatchRole): MatchPlayer = MatchPlayer(role, stats)
    def discordId: UserId = stats.discordId
  }
}

class QueueService {

  private var queue: Seq[QueuedPlayer] = Seq.empty
  private var priorityQueue: Seq[QueuedPlayer] = Seq.empty

  def getNormalQueue: Seq[QueuedPlayer] = queue

  def getPriorityQueue: Seq[QueuedPlayer] = priorityQueue

  def getRandomN(n: Int): Seq[QueuedPlayer] = Option.when(n > 0)(scala.util.Random.shuffle(queue).take(n)).getOrElse(Seq.empty)


  def upsertNormalPlayer(player: QueuedPlayer): Boolean = {
    val res = this.remove(player.stats.discordId)
    queue = queue.appended(player)
    res
  }

  def upsertPriorityPlayer(player: QueuedPlayer): Boolean = {
    val res = this.remove(player.stats.discordId)
    priorityQueue = priorityQueue.appended(player)
    res
  }

  def removeNormalPlayer(playerId: UserId): Boolean = {
    if (this.containsNormalPlayer(playerId)) {
      queue = queue.filterNot(_.stats.discordId == playerId)
      true
    } else {
      false
    }
  }

  def removePriorityPlayer(playerId: UserId): Boolean = {
    if (this.containsPriorityPlayer(playerId)) {
      priorityQueue = priorityQueue.filterNot(_.stats.discordId == playerId)
      true
    } else {
      false
    }
  }

  def remove(playerId: UserId): Boolean = {
    if (this.contains(playerId)) {
      queue = queue.filterNot(_.stats.discordId == playerId)
      priorityQueue = priorityQueue.filterNot(_.stats.discordId == playerId)
      true
    } else {
      false
    }
  }

  def containsNormalPlayer(playerId: UserId): Boolean = queue.exists(_.stats.discordId == playerId)

  def containsPriorityPlayer(playerId: UserId): Boolean = priorityQueue.exists(_.stats.discordId == playerId)

  def contains(userId: UserId): Boolean = containsNormalPlayer(userId) || containsPriorityPlayer(userId)

  def clearNormalQueue(): Unit = queue = Seq.empty

  def clearPriorityQueue(): Unit = priorityQueue = Seq.empty

  def clearAll(): Unit = {
    clearNormalQueue()
    clearPriorityQueue()
  }

  def priorityQueueSize: Int = priorityQueue.length

  def normalQueueSize: Int = priorityQueue.length

  def queueSize: Int = normalQueueSize + priorityQueueSize

  def remaining(o: OngoingMatch): Seq[QueuedPlayer] = {
    val inGamePlayers = o.team1.seq ++ o.team2.seq
    val prioPlayers = getPriorityQueue.filterNot(p => inGamePlayers.exists(_.state.discordId == p.stats.discordId))
    val queuePlayers = getNormalQueue.filterNot(p => inGamePlayers.exists(_.state.discordId == p.stats.discordId))
    prioPlayers ++ queuePlayers
  }

  def findPlayer(userId: UserId): Option[QueuedPlayer] = {
    queue.find(_.stats.discordId == userId)
      .orElse(priorityQueue.find(_.stats.discordId == userId))
  }

  def getAll(players: Seq[UserId]): Seq[QueuedPlayer] = {
    players.flatMap(p => findPlayer(p))
  }

  def updatePriorities(playingPlayers: Seq[MatchPlayer], remainingPlayers: Seq[QueuedPlayer]): Unit = {
    getAll(playingPlayers.map(_.state.discordId)).map(p => upsertNormalPlayer(p))
    remainingPlayers.map(p => upsertPriorityPlayer(p))
  }
}
