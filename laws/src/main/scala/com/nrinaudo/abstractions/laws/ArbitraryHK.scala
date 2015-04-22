package com.nrinaudo.abstractions.laws

import com.nrinaudo.abstractions.Applicative
import org.scalacheck.Arbitrary

object ArbitraryHK {
  /** Provides arbitrary instances of `F[A]`, provided implicit arbitraries are found for `A` and `F`. */
  implicit def arbFA[F[_], A](implicit arbF: ArbitraryHK[F], arbA: Arbitrary[A]): Arbitrary[F[A]] = arbF.arbitrary[A]
}

/** Used to create arbitrary instances of higher kinded types.
  *
  * This trait is especially useful when writing generic tests for type classes. Take, for example, covariant functors:
  * All implementations of the `Functor` type class *must* follow the identity law. It should therefore be possible to
  * write a generic `identity law` test case independent of the underlying implementation.
  */
trait ArbitraryHK[F[_]] {
  /** Given a type `A` and a mechanism for creating arbitrary instances of `A`,
    * creates arbitrary instances of `F[A]`.
    */
  def arbitrary[A: Arbitrary]: Arbitrary[F[A]]
}
