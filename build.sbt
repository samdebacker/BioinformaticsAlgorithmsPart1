organization := "iReact.io"

name := "Bioinformatics Algorithms Part 1"

version := "1.0"

scalaVersion := "2.11.5"

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies += "org.scalatest"  %% "scalatest"      % "2.2.1" % "test"
libraryDependencies ++= Seq(
                       "org.scalanlp"   %% "breeze"         % "0.10"
//                    ,"org.scalanlp"   %% "breeze-natives" % "0.10"
)
libraryDependencies += "io.spray"       %% "spray-json"     % "1.3.1"

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// display test duration
testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false

//////////////////////////////////////////////////////////////////
//// SETUP AMMONITE
//// https://github.com/lihaoyi/Ammonite
////
//libraryDependencies += "com.lihaoyi"    %% "ammonite"  % "0.1.6"
//
//initialCommands in console += """
//import ammonite.all._
//"""
