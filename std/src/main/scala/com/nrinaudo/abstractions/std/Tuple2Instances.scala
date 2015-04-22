package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions._
import com.nrinaudo.abstractions.ops._
import simulacrum.op

trait Tuple2Instances {
  // - Semigroup -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Semigroup instance for tuples of types that have semigroups. */
  trait Tuple2Semigroup[F, G] extends Semigroup[(F, G)] {
    implicit def F: Semigroup[F]
    implicit def G: Semigroup[G]

    override def append(a: (F, G), b: (F, G)) = (a._1 |+| b._1, a._2 |+| b._2)
  }

  implicit def tuple2Semigroup[F: Semigroup, G: Semigroup]: Semigroup[(F, G)] = new Tuple2Semigroup[F, G] {
    override val F = Semigroup[F]
    override val G = Semigroup[G]
  }


  // - Monoid -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  trait Tuple2Monoid[F, G] extends Tuple2Semigroup[F, G] with Monoid[(F, G)] {
    implicit def F: Monoid[F]
    implicit def G: Monoid[G]

    override def empty: (F, G) = (F.empty, G.empty)
  }

  implicit def tuple2Monoid[F: Monoid, G: Monoid]: Monoid[(F, G)] = new Tuple2Monoid[F, G] {
    override val F = Monoid[F]
    override val G = Monoid[G]
  }



  // - Functors --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit def tuple2Instances[X]: Functor[(X, ?)] = new Functor[(X, ?)] {
    override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))
  }
}
