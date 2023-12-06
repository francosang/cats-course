name := "cats"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.9.0"
val s2Version = "3.9.3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "co.fs2" %% "fs2-core" % s2Version
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)
