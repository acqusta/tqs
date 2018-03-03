organization    := "tquant"

name            := "tqs-stralet"

version         := "1.0-SNAPSHOT"

//scalaVersion    := "2.11.8"

javacOptions   ++= Seq("-encoding", "UTF-8")


libraryDependencies += "ch.qos.logback" % "logback-core"    % "1.2.3"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "org.slf4j"      % "slf4j-api"       % "1.7.25"

libraryDependencies += "org.zeromq"                    % "jeromq"               % "0.4.2"

val jacksonVersion = "2.9.1"
libraryDependencies += "com.fasterxml.jackson.core"       % "jackson-core"              % jacksonVersion
libraryDependencies += "com.fasterxml.jackson.core"       % "jackson-annotations"       % jacksonVersion
libraryDependencies += "com.fasterxml.jackson.module"    %% "jackson-module-scala"      % jacksonVersion
libraryDependencies += "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv"    % jacksonVersion

libraryDependencies += "org.msgpack"                      % "jackson-dataformat-msgpack" % "0.8.13"

libraryDependencies += "commons-cli" % "commons-cli" % "1.3"

val akkaVersion  = "2.5.6"
libraryDependencies += "com.typesafe.akka"  %% "akka-actor"              % akkaVersion
libraryDependencies += "com.typesafe.akka"  %% "akka-slf4j"              % akkaVersion
//libraryDependencies += "com.typesafe.akka"  %% "akka-testkit"            % akkaVersion
