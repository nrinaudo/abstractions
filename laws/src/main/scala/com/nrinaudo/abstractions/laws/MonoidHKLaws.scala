package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{MonoidHK, Eq}
import com.nrinaudo.abstractions.ops._
import com.nrinaudo.abstractions.laws.ArbitraryHK._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object MonoidHKLaws {
  def apply[F[_]: ArbitraryHK, A: Arbitrary](implicit eqF: Eq[F[A]]): MonoidHKLaws[F, A] = new MonoidHKLaws[F, A] {
    override val arbF = implicitly[ArbitraryHK[F]]
    override val arbA = implicitly[Arbitrary[A]]
    override val eqFA = eqF
  }
}

trait MonoidHKLaws[F[_], A] extends SemigroupHKLaws[F, A] {
  implicit def arbF: ArbitraryHK[F]
  implicit def arbA: Arbitrary[A]
  implicit def eqFA: Eq[F[A]]

  def plusEmpty(implicit F: MonoidHK[F]) = {
    import F._

    new DefaultRuleSet("plus", Some(plus),
      ("append identity", forAll { a: F[A] => (a |+| empty) === (empty |+| a) })
    )
  }
}
