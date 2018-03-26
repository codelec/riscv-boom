organization := "edu.berkeley.cs"

version := "1.0"

name := "boom"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) ++
			Seq("edu.berkeley.cs" %% "chisel-iotesters" % "latest.release")
libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.5.3")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val chisel = (project in file("chisel3"))
lazy val firrtl = project.dependsOn(chisel)
lazy val firrtl_interpreter = (project in file("firrtl-interpreter")).dependsOn(firrtl)
lazy val chisel_testers = (project in file("chisel-testers")).dependsOn(chisel)
lazy val riscvboom = (project in file(".")).dependsOn(firrtl_interpreter,chisel_testers)

