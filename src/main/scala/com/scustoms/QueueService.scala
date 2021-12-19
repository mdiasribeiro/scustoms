package com.scustoms

import ackcord.data.UserId
import com.scustoms.QueueService.{ExtendedQueuedPlayer, QueuedPlayer}
import com.scustoms.database.DatabaseService
import com.scustoms.database.keepers.PlayerKeeper.Player

import scala.concurrent.{ExecutionContext, Future}

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

  sealed trait Role
  final case object Top extends Role
  final case object Jungle extends Role
  final case object Mid extends Role
  final case object Bot extends Role
  final case object Support extends Role
  final case object Fill extends Role

  def parseRole(role: String): Option[Role] = role.toLowerCase match {
    case "top"               => Some(Top)
    case "jungle"            => Some(Jungle)
    case "mid"               => Some(Mid)
    case "bot"               => Some(Bot)
    case "sup" | "support"   => Some(Support)
    case "fill" | "any"      => Some(Fill)
    case _                   => None
  }

  case class QueuedPlayer(discordId: UserId, role: Role)
  case class ExtendedQueuedPlayer(discordId: UserId, role: Role, player: Player)
}

class QueueService {

  private var queue: Seq[QueuedPlayer] = Seq.empty

  def get: Seq[QueuedPlayer] = queue

  def add(player: QueuedPlayer): Boolean =
    if (this.contains(player.discordId)) {
      false
    } else {
      queue = queue.appended(player)
      true
    }

  def remove(playerId: UserId): Boolean =
    if (this.contains(playerId)) {
      queue = queue.filterNot(_.discordId == playerId)
      true
    } else {
      false
    }

  def contains(playerId: UserId): Boolean = queue.exists(_.discordId == playerId)

  def clear(): Unit = queue = Seq.empty

  def length: Int = queue.length

  def extendedInfo(implicit ec: ExecutionContext): Future[Seq[ExtendedQueuedPlayer]] = {
    val currentPlayers = queue
    DatabaseService.playerKeeper
      .findAll(currentPlayers.map(_.discordId))
      .map(allPlayers => {
        currentPlayers
          .sortBy(_.discordId.toUnsignedLong)
          .zip(allPlayers.sortBy(_.discordId.toUnsignedLong))
          .map {
            case (queuedPlayer, dbPlayer) => ExtendedQueuedPlayer(queuedPlayer.discordId, queuedPlayer.role, dbPlayer)
          }
      })
      .recover {
        case error =>
          println(s"Error occurred: ${error.getMessage}")
          Seq.empty
      }
  }
}
