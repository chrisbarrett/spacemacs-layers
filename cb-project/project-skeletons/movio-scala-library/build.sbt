name := "__PROJECT-NAME__"
organization := "mm"
scalaVersion := "__SCALA-VERSION__"

resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "movio" at "__MOVIO-ARTIFACTORY-REPO-URL__"
)

libraryDependencies ++= Seq(
  "mm" %% "esdomainlib" % "__ESDOMAINLIB-VERSION__",
  "org.scalatest" %% "scalatest" % "__SCALATEST-VERSION__" % Test
)

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-feature"
)

parallelExecution := false
fork in Test := true

releaseSettings
ReleaseKeys.versionBump := sbtrelease.Version.Bump.Minor
ReleaseKeys.tagName := version.value.toString

publishTo := {
  val repo = "__MOVIO-ARTIFACTORY-URL__"
  if (isSnapshot.value)
    Some("movio snapshots" at repo + "libs-snapshot-local")
  else
    Some("movio releases" at repo + "libs-release-local")
}
