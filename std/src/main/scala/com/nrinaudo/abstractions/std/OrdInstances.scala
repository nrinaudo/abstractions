package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions.{Ord, Semigroup}

trait OrdInstances {
  def Min[A: Ord]: Semigroup[A] = new Semigroup[A] {
    override def append(a: A, b: A): A = Ord[A].min(a, b)
  }
}
