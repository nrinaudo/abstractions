package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{SemigroupHK, Eq}
import com.nrinaudo.abstractions.ops._
import com.nrinaudo.abstractions.laws.ArbitraryHK._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import org.scalacheck.Prop._

object SemigroupHKLaws {
  def apply[F[_]: ArbitraryHK, A: Arbitrary](implicit eqF: Eq[F[A]]): SemigroupHKLaws[F, A] = new SemigroupHKLaws[F, A] {
    override val arbF = implicitly[ArbitraryHK[F]]
    override val arbA = implicitly[Arbitrary[A]]
    override val eqFA = eqF
  }
}

trait SemigroupHKLaws[F[_], A] extends Laws {
  implicit def arbF: ArbitraryHK[F]
  implicit def arbA: Arbitrary[A]
  implicit def eqFA: Eq[F[A]]

  def plus(implicit F: SemigroupHK[F]) = new DefaultRuleSet("plus", None,
    ("append associativity", forAll { (a: F[A], b: F[A], c: F[A]) =>  ((a |+| b) |+| c) === (a |+| (b |+| c)) })
  )
}
