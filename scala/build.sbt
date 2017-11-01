name          := "tqs-scala"

//scalaVersion    := "2.12.8"

javacOptions ++= Seq("-encoding", "UTF-8")

lazy val tqs = project in file("tqs")

lazy val demo = project in file("demo")      dependsOn(tqs)

//val runServer = taskKey[Unit]("Runs server")

//runServer := (run in Compile in server).toTask("").value
//val runDemo := (run in Compile in demo).toTask("").value

lazy val root = project.in( file("."))
	.aggregate(tqs, demo)
	.settings( aggregate in update := false	)

