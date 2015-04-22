package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{Eq, Semigroup}
import com.nrinaudo.abstractions.ops._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

object SemigroupLaws {
  def apply[S, A: Arbitrary: Eq]: SemigroupLaws[S, A] = new SemigroupLaws[S, A] {
    override val arbA = implicitly[Arbitrary[A]]
    override val eqA  = implicitly[Eq[A]]
  }
}

trait SemigroupLaws[S, A] extends Laws {
  implicit def arbA: Arbitrary[A]
  implicit def eqA: Eq[A]

  def semigroup(implicit S: Semigroup[A]) = new DefaultRuleSet("semigroup", None,
    ("append associativity", forAll { (a: A, b: A, c: A) =>  ((a |+| b) |+| c) === (a |+| (b |+| c)) })
  )
}
