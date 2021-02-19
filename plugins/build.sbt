ThisBuild / scalaVersion := "2.12.12"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    organization := "com.auto.plugin",
    name := "auto-plugin",
    version := "1.0"
  )
