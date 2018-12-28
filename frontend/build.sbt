enablePlugins(ScalaJSPlugin)

name := "NaiveExprFrontend"
scalaVersion := "2.12.7"

//lazy val naiveExpr = RootProject(file("../"))

//lazy val core = ProjectRef(file("../"), "core")

//lazy val frontend = (project in file(".")).dependsOn(core)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
