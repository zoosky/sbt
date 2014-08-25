lazy val check = taskKey[Unit]("Runs the check")

def commonSettings: Seq[Def.Setting[_]] =
  Seq(
    ivyPaths := new IvyPaths( (baseDirectory in ThisBuild).value, Some((target in LocalRootProject).value / "ivy-cache")),
    libraryDependencies := Seq(
      "net.sf.json-lib" % "json-lib" % "2.4" classifier "jdk15" intransitive(),
      "com.typesafe.akka" %% "akka-remote" % "2.3.4" exclude("com.typesafe.akka", "akka-actor_2.10"),
      "commons-io" % "commons-io" % "1.4"
    ),
    updateOptions := updateOptions.value.withCustomResolution(true),
    scalaVersion := "2.10.4"
  )

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    organization in ThisBuild := "org.example",
    version in ThisBuild := "1.0",
    check := {
    }
  )

