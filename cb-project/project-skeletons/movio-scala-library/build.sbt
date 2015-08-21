name := "__PROJECT-NAME__"
organization := "mm"
scalaVersion := "__SCALA-VERSION__"

resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "movio" at "__MOVIO-ARTIFACTORY-REPO-URL__"
)

libraryDependencies ++= Seq(
  // "joda-time" % "joda-time" % "__JODA-TIME-VERSION__",
  // "org.joda" % "joda-convert" % "__JODA-CONVERT-VERSION__",
  // "com.sksamuel.elastic4s" %% "elastic4s" % "__ELASTIC4S-VERSION__",
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

publishTo <<= version { (v: String) ⇒
  val repo = "__MOVIO-ARTIFACTORY-URL__"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("movio snapshots" at repo + "libs-snapshot-local")
  else
    Some("movio releases" at repo + "libs-release-local")
}
