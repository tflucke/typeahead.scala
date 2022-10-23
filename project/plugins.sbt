val ScalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.11.0")

addSbtPlugin("org.scala-js"         % "sbt-scalajs"               % ScalaJSVersion)
