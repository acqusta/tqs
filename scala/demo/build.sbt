organization    := "tquant"

name            := "tqs-demo"

version         := "1.0-SNAPSHOT"

//scalaVersion    := "2.11.8"

javacOptions   ++= Seq("-encoding", "UTF-8")

libraryDependencies += "tquant"         %% "tqs-stralet"    % "1.0-SNAPSHOT"

libraryDependencies += "com.tictactec" % "ta-lib" % "0.4.0"

libraryDependencies += "commons-cli" % "commons-cli" % "1.3"

packMain        := Map(
    "rbreaker" -> "cta.RBreakerStralet",
    "lsma"     -> "cta.LongShortMAStralet",
    "onetick"  -> "cta.OneTickStralet"
)

//packJvmOpts     := Map("tqs-backtest" -> Seq(
//  "-Djava.library.path=${PROG_HOME}/lib"
//))

packResourceDir += (baseDirectory.value / "etc" -> "etc")

packGenerateWindowsBatFile := true
