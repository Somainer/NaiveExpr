name := "NaiveExpr"


val commonSettings = Seq(
  version := "1.3",
  scalaVersion := "2.12.7",
  organization := "moe.roselia"
)

commonSettings

scalaVersion := "2.12.7"

organization := "moe.roselia"

import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

//libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.3"

//run <<= run in Compile in core

//lazy val protocol = (project in file("protocol")) settings(
//  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
//)

lazy val protocol = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("protocol")) settings(
  libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1"
)


//lazy val macros = (project in file("macros")) dependsOn protocol settings (
//  commonSettings,
//  libraryDependencies ++= Seq(
//    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
//    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
//  )
//)

lazy val core = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("core")).dependsOn(protocol).settings(
  commonSettings,
  libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1"
)

lazy val interface = (project in file("interface")) dependsOn core.jvm settings commonSettings settings (
  mainClass in assembly := Some("sample.Main")
)
//enablePlugins(ScalaJSPlugin)

lazy val frontend = (project in file("frontend")).dependsOn(core.js).enablePlugins(ScalaJSPlugin).settings(
  libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "latest.release",
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6",
  libraryDependencies += "com.thoughtworks.binding" %%% "futurebinding" % "latest.release",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

/*


lazy val bar =
// select supported platforms
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)

lazy val barJS = bar.js
lazy val barJVM = bar.jvm
*/
run := run in Compile in interface

//scalaJSUseMainModuleInitializer := true

//mainClass := Some("frontend.NaiveExprApp")
