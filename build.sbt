name := "devsearch-ast"

version := "0.1"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.github.javaparser" % "javaparser-core" % "2.0.0",
  "io.spray" %%  "spray-json" % "1.3.1",
  "com.chuusai" % "shapeless_2.10.4" % "2.0.0",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test"
)

libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.3"
