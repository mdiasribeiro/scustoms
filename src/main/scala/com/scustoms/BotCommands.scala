package com.scustoms

import ackcord.commands.{CommandController, NamedCommand}
import ackcord.requests.Requests
import akka.NotUsed
import ackcord.syntax.TextChannelSyntax

import scala.util.Random

class BotCommands(requests: Requests) extends CommandController(requests) {

  final case class User(id: Long, name: String, lines: Vector[String])

  val userMap = Vector(
    User(349606871210000385L, "Giwinho", Vector("Elo: 1200 (se for the Ashe meio, é 300)")),
    User(297130307314778112L, "States", Vector.empty),
    User(183728707368648704L, "Batatas", Vector.empty),
    User(169131637269987328L, "Ramos", Vector("Larga o LoL e vai mudar a fralda da Madalena")),
    User(298087965392109570L, "Krop", Vector.empty),
    User(298087495650902016L, "Shikishoku", Vector.empty),
    User(515150225434738700L, "Paulo", Vector.empty),
    User(168528530559336448L, "Sakini", Vector.empty),
    User(296761425982914581L, "Careca", Vector.empty),
    User(138822865708515329L, "H4uZ", Vector.empty),
    User(164791190208774144L, "kn0x", Vector.empty),
    User(168517789483532288L, "MauZaum", Vector.empty)
  )
  val commandSymbols = Seq("$")
  val randomGenerator = new Random()

  val hello: NamedCommand[NotUsed] = Command
    .named(Seq("$"), Seq("hello"))
    .withRequest(m => {
      val withEmail = m.user.email.map(email => s", I'll spam you at $email").getOrElse("")
      m.textChannel.sendMessage(s"Hello ${m.user.username}$withEmail!")
    })

  val info: NamedCommand[NotUsed] = Command
    .named(Seq("$"), Seq("info"))
    .withRequest(m => {
      val messageToSend = userMap.find(u => u.id.toString == m.user.id.toString) match {
        case Some(User(id, name, lines)) if lines.isEmpty =>
          s"Não sei nada sobre ti, $name"
        case Some(User(id, name, lines)) =>
          s"${lines(randomGenerator.nextInt(lines.size))}"
        case None =>
          s"Não te conheço de lado nenhum"
      }
      m.textChannel.sendMessage(messageToSend)
    })
}
