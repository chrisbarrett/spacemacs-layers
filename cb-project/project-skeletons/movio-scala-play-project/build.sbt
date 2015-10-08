name := "__PROJECT-NAME__-parent"

lazy val commonSettings = Seq(
  organization := "mm",
  scalaVersion := "__SCALA-VERSION__",

  resolvers ++= Seq(
    "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
    "movio" at "__MOVIO-ARTIFACTORY-REPO-URL__"
  ),

  scalacOptions ++= Seq(
    "-Xlint",
    "-deprecation",
    "-feature"
  ),

  publishTo := {
    val repo = "__MOVIO-ARTIFACTORY-URL__"
    if (isSnapshot.value)
      Some("movio snapshots" at repo + "libs-snapshot-local")
    else
      Some("movio releases" at repo + "libs-release-local")
  },

  fork in Test := true
) ++ releaseSettings

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish := {})
  .aggregate(service, client)

lazy val service = project.in(file("service"))
  .settings(commonSettings: _*)
  .dependsOn(client)
  .settings(
    name := "__PROJECT-NAME__",
    libraryDependencies ++= Seq(
      "com.softwaremill.macwire" %% "macros" % "__MACWIRE-VERSION__",
      "com.softwaremill.macwire" %% "runtime" % "__MACWIRE-VERSION__",
      "mm" %% "playlib" % "__PLAYLIB-VERSION__",
      "mm" %% "esdomainlib" % "__ESDOMAINLIB-VERSION__",
      "org.scalatestplus" %% "play" % "__SCALATESTPLUS-VERSION__" % Test
    ),
    javaOptions in Test += "-Dconfig.file=test/resources/application.conf",
    javaOptions in Test += "-Dlogger.file=test/resources/logback.xml",

    routesGenerator := play.routes.compiler.InjectedRoutesGenerator
  )
  .enablePlugins(PlayScala)

lazy val client = project.in(file("client"))
  .settings(commonSettings: _*)
  .settings(
    name := "__PROJECT-NAME__-client",
    libraryDependencies ++= Seq(
      json
    )
  )

name in Universal := moduleName.value

ReleaseKeys.versionBump := sbtrelease.Version.Bump.Minor
ReleaseKeys.tagName := version.value.toString
