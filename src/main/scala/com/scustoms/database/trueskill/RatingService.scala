package com.scustoms.database.trueskill

import ackcord.data.UserId
import com.scustoms.database.keepers.PlayerKeeper.Player
import de.gesundkrank.jskills.{GameInfo, IPlayer, ITeam, Rating}
import de.gesundkrank.jskills.trueskill.TwoTeamTrueSkillCalculator

import java.util
import scala.jdk.CollectionConverters._

object RatingService {

  class ScustomsTeam extends util.HashMap[IPlayer, Rating] with ITeam

  val calculator = new TwoTeamTrueSkillCalculator()
  val gameInfo: GameInfo = GameInfo.getDefaultGameInfo
  val player1Rating: Rating = new Rating(gameInfo.getInitialMean, gameInfo.getInitialStandardDeviation)
  val player1: IPlayer = Player(1, UserId(138822865708515329L), "H4uZ", "H4uZ", player1Rating)
  val player6Rating: Rating = new Rating(gameInfo.getInitialMean, gameInfo.getInitialStandardDeviation)
  val player6: IPlayer = Player(6, UserId(349606871210000385L), "Giwinho", "Giwinho", player6Rating)
  val team1: ITeam = new ScustomsTeam()
  team1.put(player1, player1Rating)
  val team2: ITeam = new ScustomsTeam()
  team2.put(player6, player6Rating)
  val teams: util.List[ITeam] = Seq(team1, team2).asJava
}
