name := "NaiveExpr"


val commonSettings = Seq(
  version := "1.3",
  scalaVersion := "2.12.7",
  organization := "moe.roselia"
)

commonSettings

scalaVersion := "2.12.7"

organization := "moe.roselia"


//libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.3"

//run <<= run in Compile in core

lazy val protocol = (project in file("protocol")) settings(
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
)

lazy val macros = (project in file("macros")) dependsOn protocol settings (
  commonSettings,
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  )
)

lazy val core = (project in file("core")).dependsOn(protocol, macros).settings(
  commonSettings,
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
)

lazy val interface = (project in file("interface")) dependsOn core settings commonSettings settings (
  mainClass in assembly := Some("sample.Main")
)

run := run in Compile in interface
