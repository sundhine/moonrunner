name := "MyProject"

version := "0.1"

scalaVersion := "2.12.3"

val monocleVersion = "1.4.0"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "com.lihaoyi" %% "pprint" % "0.5.2")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")