import Dependencies._

ThisBuild / scalaVersion := "2.13.6"

lazy val root = (project in file("."))
  .settings(name := "fs2-guide", libraryDependencies ++= Seq(fs2))

addCompilerPlugin(
  ("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)
)
