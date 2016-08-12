lazy val genome = project
  .in(file("."))
  .enablePlugins(AutomateHeaderPlugin, GitVersioning)

libraryDependencies ++= Vector(
  Library.cats,
  Library.scalaTest % "test"
)

initialCommands := """|import com.perevillega._
                      |""".stripMargin
