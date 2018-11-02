name := "retirement_calculator"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

mainClass in Compile := Some("com.plusonetesting.retcalc.SimulatePlanApp")