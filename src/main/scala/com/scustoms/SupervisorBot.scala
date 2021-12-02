package com.scustoms

import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Terminated}
import akka.actor.typed.scaladsl.Behaviors

object SupervisorBot {
  def apply(): Behavior[NotUsed] =
    Behaviors.setup { context =>
      val testBot = context.spawn(TestBot(), "test_bot")
      context.watch(testBot)

      Behaviors.receiveSignal {
        case (_, Terminated(_)) =>
          Behaviors.stopped
      }
    }
}