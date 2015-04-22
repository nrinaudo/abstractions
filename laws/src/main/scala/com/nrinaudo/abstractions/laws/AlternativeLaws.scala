package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{Alternative, Eq}
import org.scalacheck.Arbitrary

object AlternativeLaws {
  def apply[F[_]: ArbitraryHK, A: Arbitrary](implicit eqF: Eq[F[A]]): AlternativeLaws[F, A] = new AlternativeLaws[F, A] {
    override val arbF = implicitly[ArbitraryHK[F]]
    override val arbA = implicitly[Arbitrary[A]]
    override val eqFA: Eq[F[A]] = eqF
  }
}

trait AlternativeLaws[F[_], A] extends ApplicativeLaws[F, A] with MonoidHKLaws[F, A] {
  def alternative[B: Arbitrary, C: Arbitrary](implicit F: Alternative[F], eqFC: Eq[F[C]]) = new RuleSet {
    override def name    = "alternative"
    override def bases   = Seq.empty
    override def props   = Seq.empty
    override def parents = Seq(plusEmpty, applicative[B, C])
  }
}
