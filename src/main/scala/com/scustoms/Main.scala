package com.scustoms

import ackcord._
import ackcord.data._
import akka.actor.typed.ActorSystem

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main extends App {
  // Start actor system and supervisor
  val system = ActorSystem(SupervisorBot(), "scustoms-system")
}
