package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions._
import com.nrinaudo.abstractions.ops._

trait OptionInstances {
  implicit def optionEq[A: Eq]: Eq[Option[A]] = Eq { (oa, ob) =>
    (oa, ob) match {
      case (Some(a), Some(b)) if a === b => true
      case (None,    None)               => true
      case _                             => false
    }
  }

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None
    override def append(oa: Option[A], ob: Option[A]): Option[A] = (oa, ob) match {
      case (Some(a), Some(b)) => Some(a |+| b)
      case (Some(_), _)       => oa
      case (_,       Some(_)) => ob
      case _                  => None
    }
  }

  /** Monoid instance that returns the first non-`None` operand. */
  val First: MonoidHK[Option] = new MonoidHK[Option] {
    override def empty[A] = None
    override def append[A](a: Option[A], b: Option[A]) = a orElse b
  }

  /** Monoid instance that returns the last non-`None` operand. */
  val Last: MonoidHK[Option] = new MonoidHK[Option] {
    override def empty[A] = None
    override def append[A](a: Option[A], b: Option[A]) = b orElse a
  }


  implicit val optionInstances = new Monad[Option] with Foldable[Option] {
    // - Functor methods -----------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa match {
      case Some(a) => Some(f(a))
      case None    => None
    }



    // - Foldable methods ----------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def foldLeft[A, B](fa: Option[A], init: B)(f: (B, A) => B): B = fa match {
      case Some(a) => f(init, a)
      case None    => init
    }

    override def foldRight[A, B](fa: Option[A], init: B)(f: (A, => B) => B): B = foldLeft(fa, init)((b, a) => f(a, b))



    // - Applicative methods -------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def pure[A](a: A): Option[A] = Some(a)
    override def ap[A, B](oa: Option[A])(of: Option[A => B]): Option[B] = (oa, of) match {
      case (Some(a), Some(f)) => Some(f(a))
      case _                  => None
    }


    // - Monad methods -------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None    => None
    }
  }

  implicit def optionTFunctor[F[_]: Functor]: Functor[OptionT[F, ?]] = new Functor[OptionT[F, ?]] {
    override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa map f
  }

  implicit def optionTMonad[F[_]: Monad]: Monad[OptionT[F, ?]] = new Monad[OptionT[F, ?]] {
    override def pure[A](a: A): OptionT[F, A] = OptionT(Monad[F].pure(Some(a)))
    override def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa flatMap f
  }
}

case class OptionT[F[_], A](run: F[Option[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = OptionT(run.map(_.map(f)))
  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(run.flatMap {
      case Some(a) => f(a).run
      case None => F.pure(None)
    })
}