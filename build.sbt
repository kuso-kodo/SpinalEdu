name := "SpinalEdu"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "1.3.5",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "1.3.5"
)

fork := true