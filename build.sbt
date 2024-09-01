val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "zio-web-tokens",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.21",
      "dev.zio" %% "zio-http" % "3.0.0-RC4",
      "dev.zio" %% "zio-json" % "0.6.2",
      "dev.zio" %% "zio-test" % "2.0.20" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.0.20" % Test,
      "dev.zio" %% "zio-http-testkit" % "3.0.0-RC4" % Test
    )
  )
