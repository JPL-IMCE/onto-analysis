
enablePlugins(JavaAppPackaging)

// Docker-compatible version strings
// See: https://github.com/dwijnand/sbt-dynver#docker-compatible-version-strings
dynverSeparator in ThisBuild := "-"

mainClass in Compile := None

name := "onto-analysis"

scalaVersion := "2.13.1"

licenses in ThisBuild := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
organization in ThisBuild := "gov.nasa.jpl.imce.caesar.onto-analysis"
organizationName in ThisBuild := "Jet Propulsion Laboratory, California Institute of Technology"

homepage in ThisBuild := Some(url("https://github.com/jpl-imce/onto-analysis"))

startYear in ThisBuild := Some(2019)

maintainer in ThisBuild := "Nicolas F. Rouquette and Steven J. Jenkins"

resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.JPL-IMCE" % "pellet" % "2.4.0-JPL-IMCE"

libraryDependencies += "io.circe" %% "circe-yaml" % "0.12.0"
libraryDependencies += "io.circe" %% "circe-optics" % "0.12.0"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.13"
