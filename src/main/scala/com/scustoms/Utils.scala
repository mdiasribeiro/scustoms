package com.scustoms

import scala.concurrent.{ExecutionContext, Future}

object Utils {
  def foldEitherOfFuture[A, B](e: Either[A, Future[B]])(implicit ec: ExecutionContext): Future[Either[A, B]] =
    e match {
      case Left(s) => Future.successful(Left(s))
      case Right(f) => f.map(Right(_))
    }

  def foldEitherOfFutureSeq[A, B](e: Either[A, Seq[Future[B]]])(implicit ec: ExecutionContext): Future[Either[A, Seq[B]]] =
    e match {
      case Left(s) => Future.successful(Left(s))
      case Right(f) => Future.sequence(f).map(seq => Right(seq))
    }

  def sequenceEither[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldLeft(Right(Nil): Either[A, Seq[B]]) {
      (acc, e) => for (x <- e; xs <- acc) yield xs :+ x
    }

  implicit class StringImprovements(val s: String) {
    def pad(padding: Int): String = {
      val str = s.trim
      if (str.length > padding - 1) {
        str.substring(0, padding - 4).appendedAll("... ")
      } else {
        str.padTo(padding, ' ')
      }
    }
  }

  implicit class SeqImprovements(val s: Seq[String]) {
    def padConcat(padding: Int): String = s.map(_.pad(padding)).reduceLeft(_ + _)

    def ifNonEmpty(f: => String): String = if (s.isEmpty) "" else f
  }
}
