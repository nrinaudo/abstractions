package com.nrinaudo

package object abstractions {
  /** Phantom type used to turn "simple kinded" types into higher kinded ones.
    *
    * It can be thought of as the type level version of `Function.const[A, B](a: A): (A, B) => A`.
    */
  type Const[M, X] = M

  type Id[A] = A

  /** Declares implicit operators for all type classes. */
  object ops extends Functor.ToFunctorOps         with
                     Eq.ToEqOps                   with
                     Apply.ToApplyOps             with
                     Applicative.ToApplicativeOps with
                     Semigroup.ToSemigroupOps     with
                     Monoid.ToMonoidOps           with
                     SemigroupHK.ToSemigroupHKOps with
                     MonoidHK.ToMonoidHKOps       with
                     FlatMap.ToFlatMapOps         with
                     Foldable.ToFoldableOps       with
                     Monad.ToMonadOps             with
                     Traverse.ToTraverseOps       with
                     Ord.ToOrdOps
}
