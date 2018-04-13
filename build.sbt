organization := "edu.berkeley.cs"

version := "1.0"

name := "boom"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) ++
			Seq("org.json4s" %% "json4s-jackson" % "3.5.3") ++
			Seq("org.scalatest" %% "scalatest" % "2.2.2" % Test)
addCompilerPlugin("org.scalamacros" % "paradise_2.11.12" % "2.1.0")
