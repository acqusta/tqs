organization    := "xtz"

name            := "tqs-stralet"

version         := "1.0-SNAPSHOT"

scalaVersion    := "2.11.8"

//resolvers       += "Local Maven Repository" at "file:///D:/java/maven/repo"
//resolvers       += "Scalaz Bintray Repo"    at "http://dl.bintray.com/scalaz/releases"


javacOptions   ++= Seq("-encoding", "UTF-8")


libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.7"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.20"

libraryDependencies += "org.zeromq"                       % "jeromq"               % "0.3.6"

libraryDependencies += "com.fasterxml.jackson.core"       % "jackson-core"         % "2.7.3"
libraryDependencies += "com.fasterxml.jackson.core"       % "jackson-annotations"  % "2.7.3"
libraryDependencies += "com.fasterxml.jackson.module"    %% "jackson-module-scala" % "2.7.3"
libraryDependencies += "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv"     % "2.7.3"

libraryDependencies += "org.msgpack"                      % "jackson-dataformat-msgpack" % "0.8.12"

libraryDependencies += "com.tictactec" % "ta-lib" % "0.4.0"

libraryDependencies += "commons-cli" % "commons-cli" % "1.3"

val akkaVersion  = "2.4.11"
libraryDependencies += "com.typesafe.akka"  %% "akka-actor"              % akkaVersion
libraryDependencies += "com.typesafe.akka"  %% "akka-slf4j"              % akkaVersion
libraryDependencies += "com.typesafe.akka"  %% "akka-testkit"            % akkaVersion

packAutoSettings

//packMain        := Map("tqs-backtest" -> "xtz.tquant.stra.backtest.Boot")

//packJvmOpts     := Map("tquant-api" -> Seq(
//  "-Djava.library.path=${PROG_HOME}/lib"
//))

//packResourceDir += (baseDirectory.value / "etc" -> "etc")

//packGenerateWindowsBatFile := true
