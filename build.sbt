val scala3Version = "3.2.1"
val zioVersion = "2.0.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "girafflux",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
      "dev.zio" %% "zio-test-intellij" % "1.0.14" % "test",
      "dev.zio" %% "zio-test-magnolia" % zioVersion % "test"
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    ,
  )
