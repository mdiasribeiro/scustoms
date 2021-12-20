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
}
