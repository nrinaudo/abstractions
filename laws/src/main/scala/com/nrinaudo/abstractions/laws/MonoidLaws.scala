package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.{Monoid, Eq}
import com.nrinaudo.abstractions.ops._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object MonoidLaws {
  def apply[S, A: Arbitrary: Eq]: MonoidLaws[S, A] = new MonoidLaws[S, A] {
      override val arbA = implicitly[Arbitrary[A]]
      override val eqA  = implicitly[Eq[A]]
    }
}

trait MonoidLaws[M, A] extends SemigroupLaws[M, A] {
  def monoid(implicit M: Monoid[A]) = {
    import M._

    new DefaultRuleSet("monoid", Some(semigroup),
      ("append identity", forAll { a: A =>
        ((empty |+| a) === a) && ((a |+| empty) === a)
      })
    )
  }
}
