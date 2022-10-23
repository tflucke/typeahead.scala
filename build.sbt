val VersionSuffix = Option(System.getenv("VERSION_SUFFIX")).getOrElse("")

lazy val compileDemo = taskKey[Unit]("Compile a demo web site.")

lazy val root = project.in(file("."))
  .aggregate(
    typeahead,
    demo
  ).settings(
    crossScalaVersions := Nil,
    publish / skip := true,
    compileDemo := Def.sequential(typeahead / Compile / fastOptJS, demo / Compile / fastOptJS).value
  )

def commonSettings = Seq(
  organization := "com.tflucke",
  //maintainer := "Thomas Flucke <admin@tflucke.name>",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  crossScalaVersions := Seq("2.13.3", "2.12.11")
)

lazy val typeahead = (project in file("typeahead"))
  .settings(commonSettings)
  .settings(
    name := "typeahead-scala",
    version := s"0.2.1$VersionSuffix",
    libraryDependencies ++= (if (scalaJSVersion.startsWith("0.6.")) Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.8"
    ) else Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.0.0"
    ))
  ).enablePlugins(ScalaJSPlugin)

lazy val demo = (project in file("demo"))
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    name := "typeahead-scala-demo",
    version := s"0.1.0$VersionSuffix",
    publish / skip := true
  ).dependsOn(typeahead)
  .enablePlugins(ScalaJSPlugin)
