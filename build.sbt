name := """Bioinformatics Algorithms Part 1"""

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

// display test duration
testOptions in Test += Tests.Argument("-oD")

//
//parallelExecution in Test := false
