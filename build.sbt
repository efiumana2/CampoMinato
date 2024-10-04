ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

libraryDependencies += "com.raquo" %%% "laminar" % "17.0.0"

// Configura il target di Scala.js
scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

// Scala.js DOM e altri utili strumenti
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.3.0"

enablePlugins(ScalaJSPlugin)

// Impostazioni per Scala.js
scalaJSUseMainModuleInitializer := true

lazy val root = (project in file("."))
  .settings(
    name := "CampoMinato"
  )