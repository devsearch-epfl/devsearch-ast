name := "devsearch-ast"

version := "0.1"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

// Common
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.7" % "test"
)

// Parser
libraryDependencies ++= Seq(
  "com.github.javaparser" % "javaparser-core" % "2.0.0",
  "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
  "org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full,
  "io.spray" %%  "spray-json" % "1.3.1"
)

// Features
libraryDependencies ++= Seq(
  "com.github.nikita-volkov" % "sext" % "0.2.3",
  "org.apache.spark" %% "spark-core" % "1.3.0"
)
