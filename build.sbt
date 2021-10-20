name := "wacc_16"

version := "0.1"
scalaVersion := "2.13.4"

javacOptions ++= Seq("-source", "14.0.2", "-target", "14.0.2")

libraryDependencies += "com.github.j-mie6" %% "parsley" % "2.8.3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test
libraryDependencies += "org.apache.commons" % "commons-text" % "1.9"
libraryDependencies += "org.scalafx" % "scalafx_2.13" % "14-R19"
libraryDependencies += "org.fxmisc.richtext" % "richtextfx" % "0.10.6"

lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m=>
  "org.openjfx" % s"javafx-$m" % "14" classifier osName
)


libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.5"
libraryDependencies += "com.kodedu.terminalfx" % "terminalfx" % "1.1.0"
libraryDependencies += "org.jetbrains.pty4j" % "pty4j" % "0.8.6"
scalacOptions += "-Ymacro-annotations"

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "terminalfx-repo" at "https://github.com/javaterminal/terminalfx/raw/master/releases"
resolvers += "pty4j-releases" at "https://jetbrains.bintray.com/pty4j"



assemblyOption in assembly := (assemblyOption in assembly).value.copy(
  assemblyDirectory = baseDirectory.value / "assembly"
)

assemblyMergeStrategy in assembly := {
  case "module-info.class" =>
    MergeStrategy.concat
  case s =>
    MergeStrategy.defaultMergeStrategy(s)
}

test in assembly := {}
mainClass in assembly := Some("WACCCompilerFrontEnd")

Global / onChangedBuildSource := ReloadOnSourceChanges