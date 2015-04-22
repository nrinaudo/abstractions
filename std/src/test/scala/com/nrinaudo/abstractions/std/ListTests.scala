package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions.laws.{AlternativeLaws, ArbitraryHK}
import com.nrinaudo.abstractions.std.ListTests._
import com.nrinaudo.abstractions.std.all._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

object ListTests {
  implicit val arbList: ArbitraryHK[List] = new ArbitraryHK[List] {
    override def arbitrary[A: Arbitrary]: Arbitrary[List[A]] = implicitly
  }
}

class ListTests extends FunSuite with Discipline {
  checkAll("List[Int]",    AlternativeLaws[List, Int].alternative[Float, Int])
  checkAll("List[String]", AlternativeLaws[List, String].alternative[Float, Int])
}