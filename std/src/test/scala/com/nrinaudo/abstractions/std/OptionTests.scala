package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions.laws._
import com.nrinaudo.abstractions.std.OptionTests._
import com.nrinaudo.abstractions.std.all._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

object OptionTests {
  implicit val arbOption: ArbitraryHK[Option] = new ArbitraryHK[Option] {
    override def arbitrary[A: Arbitrary]: Arbitrary[Option[A]] = implicitly
  }
}

class OptionTests extends FunSuite with Discipline {
  checkAll("Option[Int]",    ApplicativeLaws[Option, Int].applicative[String, Int])
  checkAll("Option[String]", ApplicativeLaws[Option, String].applicative[Float, Int])
}