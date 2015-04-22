package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{Eq, Functor}
import com.nrinaudo.abstractions.ops._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws
import ArbitraryHK._

object FunctorLaws {
  def apply[F[_]: ArbitraryHK, A: Arbitrary](implicit eqF: Eq[F[A]]): FunctorLaws[F, A] = new FunctorLaws[F, A] {
    override val arbF = implicitly[ArbitraryHK[F]]
    override val arbA = implicitly[Arbitrary[A]]
    override val eqFA = eqF
  }
}

/** Tests the laws that a valid `Functor` instance must obey. */
trait FunctorLaws[F[_], A] extends Laws {
  implicit def arbF: ArbitraryHK[F]
  implicit def eqFA: Eq[F[A]]
  implicit def arbA: Arbitrary[A]

  def functor[B: Arbitrary, C: Arbitrary](implicit F: Functor[F], eqFC: Eq[F[C]]) =
    new SimpleRuleSet("functor",
      ("covariant functor identity", {
        forAll { fa: F[A] => fa.map(identity) === fa }
      }),
      ("covariant functor composition", {
        forAll { (fa: F[A], f: A => B, g: B => C) =>
          fa.map(f).map(g) === fa.map(g compose f)
        }
      })
    )
}
