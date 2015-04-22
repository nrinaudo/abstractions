package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions._
import com.nrinaudo.abstractions.ops._

trait ListInstances {
  implicit def listEq[A: Eq]: Eq[List[A]] = Eq { (xs, ys) => xs.corresponds(ys)(_ === _) }

  implicit val listInstances = new Alternative[List] with Foldable[List] {
    // - Functor methods -----------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def map[A, B](as: List[A])(f: A => B) = as match {
      case h :: t => f(h) :: map(t)(f)
      case Nil    => Nil
    }



    // - Applicative methods -------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def pure[A](a: A) = List(a)

    override def ap[A, B](as: List[A])(fs: List[A => B]) = as.flatMap(a => fs.map(_(a)))



    // - Foldable methods ----------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def foldLeft[A, B](fa: List[A], init: B)(f: (B, A) => B): B = {
      def step(fa: List[A], acc: B): B = fa match {
        case h :: t => step(t, f(acc, h))
        case Nil    => acc
      }

      step(fa, init)
    }

    override def foldRight[A, B](fa: List[A], init: B)(f: (A, => B) => B): B = fa match {
      case h :: t => f(h, foldRight(t, init)(f))
      case Nil    => init
    }


    // - Monoid methods ------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    override def empty[A] = Nil
    override def append[A](a: List[A], b: List[A]) = a ++ b
  }
}
