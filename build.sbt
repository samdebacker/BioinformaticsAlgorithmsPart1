name := """Bioinformatics Algorithms Part 1"""

version := "1.0"

scalaVersion := "2.11.4"
//scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest"      % "2.2.1" % "test",
  "org.scalanlp"  %% "breeze"         % "0.10"
  //,"org.scalanlp"  %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

// display test duration
testOptions in Test += Tests.Argument("-oD")

//parallelExecution in Test := false
