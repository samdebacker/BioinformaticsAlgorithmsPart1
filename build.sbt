name := """Bioinformatics Algorithms Part 1"""

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"      % "2.2.1" % "test",
  "org.scalanlp"  %% "breeze"         % "0.10",
  //,"org.scalanlp"  %% "breeze-natives" % "0.10"
  "io.spray"      %% "spray-json"     % "1.3.1"
)

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// display test duration
testOptions in Test += Tests.Argument("-oD")

//parallelExecution in Test := false
