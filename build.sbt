name := "devsearch-ast"

shellPrompt := { state => "[\033[36m" + name.value + "\033[0m] $ " }

version := "0.1"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

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
  "com.github.nikita-volkov" % "sext" % "0.2.3"
)

//parallelExecution in Test := false
