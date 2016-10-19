val scalaV = "2.11.8"

organization := "pl.nfalek"

name := "scala-playground"

scalaVersion := scalaV

libraryDependencies ++= {
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  )
}
