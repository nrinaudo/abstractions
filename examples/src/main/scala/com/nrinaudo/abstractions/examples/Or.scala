package com.nrinaudo.abstractions.examples

import com.nrinaudo.abstractions.common.{Or, OrT}
import com.nrinaudo.abstractions.std.all._

object OrExamples extends App {
  // - Simple example --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def add(sa: String, sb: String): Or[Exception, Int] = for {
    a <- Or(sa.toInt)
    b <- Or(sb.toInt)
  } yield a + b


  println(add("1", "2"))
  println(add("1", "foo"))


  // - Monad transformer -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def addOpt(oa: Option[String], ob: Option[String]): Option[Or[Exception, Int]] = (for {
    a <- OrT(oa.map(sa => Or(sa.toInt)))
    b <- OrT(ob.map(sb => Or(sb.toInt)))
  } yield a + b).run

  println(addOpt(Some("1"), Some("2")))
  println(addOpt(Some("1"), Some("foo")))
  println(addOpt(None, Some("foo")))
}
