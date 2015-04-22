package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{Applicative, Eq}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import ArbitraryHK._
import com.nrinaudo.abstractions.ops._

object ApplicativeLaws {
  def apply[F[_]: ArbitraryHK, A: Arbitrary](implicit eqF: Eq[F[A]]): ApplicativeLaws[F, A] = new ApplicativeLaws[F, A] {
    override val arbF = implicitly[ArbitraryHK[F]]
    override val arbA = implicitly[Arbitrary[A]]
    override val eqFA = eqF
  }
}

trait ApplicativeLaws[F[_], A] extends ApplyLaws[F, A] {
  implicit def arbF: ArbitraryHK[F]
  implicit def eqFA: Eq[F[A]]
  implicit def arbA: Arbitrary[A]

  def applicative[B: Arbitrary, C: Arbitrary](implicit F: Applicative[F], eqFC: Eq[F[C]]) = {
    import F._

    new DefaultRuleSet("applicative", Some(apply[B, C]),
      // pure id <*> v = v
      ("applicative identity", forAll { fa: F[A] => (fa <*> pure((a: A) => a)) === fa}),

      // pure f <*> pure x = pure (f x)
      ("applicative homomorphism", forAll { (a: A, f: A => C) =>
        (pure(a) <*> pure(f)) === pure(f(a))
      }),

      // u <*> pure y = pure ($ y) <*> u
      ("applicative interchange", forAll { (a: A, f: F[A => C]) =>
        (pure(a) <*> f) === (f <*> pure((f: A => C) => f(a)))
      }),

      // u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
      ("applicative composition", forAll { (fa: F[A], fab: F[A => B], fbc: F[B => C]) =>
        ((fa <*> fab) <*> fbc) === (fa <*> (fab <*> (fbc <*> pure((bc: B => C) => (ab: A => B) => ab andThen bc))))
      })
    )
  }
}
