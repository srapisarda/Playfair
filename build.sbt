name := "payfair"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Classpaths.typesafeResolver

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
 
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature" )

