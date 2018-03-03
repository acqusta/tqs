organization    := "tquant"

name            := "tqs-demo"

version         := "1.0-SNAPSHOT"

//scalaVersion    := "2.11.8"

javacOptions   ++= Seq("-encoding", "UTF-8")

libraryDependencies += "tquant"         %% "tqs-stralet"    % "1.0-SNAPSHOT"

val jacksonVersion = "2.9.1"
libraryDependencies += "com.fasterxml.jackson.core"       % "jackson-core"              % jacksonVersion
libraryDependencies += "com.fasterxml.jackson.core"       % "jackson-annotations"       % jacksonVersion
libraryDependencies += "com.fasterxml.jackson.module"    %% "jackson-module-scala"      % jacksonVersion
libraryDependencies += "com.fasterxml.jackson.dataformat" % "jackson-dataformat-csv"    % jacksonVersion


libraryDependencies += "com.tictactec" % "ta-lib" % "0.4.0"

libraryDependencies += "commons-cli" % "commons-cli" % "1.3"

packMain        := Map(
    "rbreaker" -> "cta.RBreakerStralet",
    "lsma"     -> "cta.LongShortMAStralet",
    "onetick"  -> "cta.OneTickStralet",
    "demo"     -> "demo.DemoStralet"
)

packJvmOpts     := Map(
	"demo" -> Seq(
  		"-Djava.library.path=${PROG_HOME}/lib"
	),

)

packResourceDir += (baseDirectory.value / "etc" -> "etc")

packGenerateWindowsBatFile := true
