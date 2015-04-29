package com.nrinaudo.abstractions.common

import com.nrinaudo.abstractions.Monad
import simulacrum.op

/** Right-biased version of {{{Either}}}. */
sealed trait Or[+A, +B] {
  def map[C](f: B => C): Or[A, C]
  def flatMap[AA >: A, C](f: B => Or[AA, C]): Or[AA, C]
}

case class Left[A](value: A) extends Or[A, Nothing] {
  override def map[C](f: Nothing => C): Or[A, C] = this
  override def flatMap[AA >: A, C](f: Nothing => Or[AA, C]): Or[AA, C] = this
}

case class Right[B](value: B) extends Or[Nothing, B] {
  override def map[C](f: B => C): Or[Nothing, C] = Right(f(value))
  override def flatMap[AA >: Nothing, C](f: B => Or[AA, C]): Or[AA, C] = f(value)
}

object Or {
  def apply[A](f: => A): Or[Exception, A] =
    try {right(f)}
    catch {
      case e: Exception => left(e)
    }

  def left[A, B](a: A): Or[A, B] = Left(a)
  def right[A, B](b: B): Or[A, B] = Right(b)
}

trait OrInstances {
  implicit def orInstances[A] = new Monad[Or[A, ?]] {
    override def flatMap[B, C](fa: Or[A, B])(f: B => Or[A, C]): Or[A, C] = fa flatMap f
    override def pure[B](b: B): Or[A, B] = Right(b)
  }
}