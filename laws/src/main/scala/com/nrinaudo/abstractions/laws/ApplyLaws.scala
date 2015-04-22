package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{Apply, Eq}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import ArbitraryHK._
import com.nrinaudo.abstractions.ops._

object ApplyLaws {
  def apply[F[_]: ArbitraryHK, A: Arbitrary](implicit eqF: Eq[F[A]]): ApplyLaws[F, A] = new ApplyLaws[F, A] {
    override val arbF = implicitly[ArbitraryHK[F]]
    override val arbA = implicitly[Arbitrary[A]]
    override val eqFA = eqF
  }
}

/** Tests for the various laws that a valid instance of {{{Apply}}} must follow. */
trait ApplyLaws[F[_], A] extends FunctorLaws[F, A] {
  implicit def arbF: ArbitraryHK[F]
  implicit def eqFA: Eq[F[A]]
  implicit def arbA: Arbitrary[A]

  def apply[B: Arbitrary, C: Arbitrary](implicit F: Apply[F], eqFC: Eq[F[C]]) =
    new DefaultRuleSet("apply", Some(functor[B, C]),
      ("apply associativity", {
        forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
          ((fa <*> fab) <*> fbc) === (fa <*> (fab <*> fbc.map((bc: B => C) => (ab: A => B) => ab andThen bc)))
        }
      })
    )
}