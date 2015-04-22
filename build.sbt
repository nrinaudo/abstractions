import com.typesafe.sbt.SbtSite.SiteKeys._

lazy val commonSettings = Seq(
  scalaVersion := "2.11.5",
  crossScalaVersions := Seq("2.11.5"),

  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
//    "-Ymacro-debug-lite",
    "-Xfuture"
  ),
  resolvers ++= Seq(
  "bintray/non" at "http://dl.bintray.com/non/maven",
  Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.3.0",
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2"),
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )
)


lazy val core = project.settings(commonSettings: _*)

lazy val laws = project.dependsOn(core)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest"  % "2.2.4",
    "org.typelevel" %% "discipline" % "0.2.1"
  ))

lazy val std = project.dependsOn(core, laws % "test").settings(commonSettings: _*)
