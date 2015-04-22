package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions.laws.MonoidLaws
import com.nrinaudo.abstractions.std.all._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ValTests extends FunSuite with Discipline {
  checkAll("Addition",       MonoidLaws[Addition.type, Int].monoid(Addition))
  checkAll("Multiplication", MonoidLaws[Multiplication.type, Int].monoid(Multiplication))

  checkAll("Conjunction", MonoidLaws[Conjunction.type, Boolean].monoid(Conjunction))
  checkAll("Disjunction", MonoidLaws[Disjunction.type, Boolean].monoid(Disjunction))
}
