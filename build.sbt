name := """Bioinformatics Algorithms Part 1"""

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"      % "2.2.1" % "test",
  "org.scalanlp"  %% "breeze"         % "0.8.1",
  "org.scalanlp"  %% "breeze-natives" % "0.8.1"
)

// display test duration
testOptions in Test += Tests.Argument("-oD")

//
//parallelExecution in Test := false
