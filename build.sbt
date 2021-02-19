name := "scala-bootcamp-hw"

version := "0.1"

scalaVersion := "2.12.12"

val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)

sbtPlugin := true
