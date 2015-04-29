package com.nrinaudo.abstractions.examples

import java.io.File
import java.util.Properties

import com.nrinaudo.abstractions.common.{ReaderT, Reader}
import com.nrinaudo.abstractions._
import com.nrinaudo.abstractions.std.all._

object ReaderExamples extends App {
  case class User(name: String, home: File)


  // - Simple example --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def userName: Reader[Properties, String] = Reader(_.getProperty("user.name"))
  def userHome: Reader[Properties, File] = Reader(p => new File(p.getProperty("user.home")))

  def user: Reader[Properties, User] = for {
    name <- userName
    home <- userHome
  } yield User(name, home)


  println(user.run(System.getProperties))


  // - Nested monads ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def optUserName: ReaderT[Option, Properties, String] = ReaderT(p => Option(p.getProperty("user.name")))
  def optUserHome: ReaderT[Option, Properties, File] = ReaderT(p => Option(p.getProperty("user.home")).map(f => new File(f)))

  def optUser: ReaderT[Option, Properties, User] = for {
    name <- optUserName
    home <- optUserHome
  } yield User(name, home)

  println(optUser.run(System.getProperties))
  println(optUser.run(new Properties()))
}
