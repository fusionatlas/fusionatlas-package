import sbt._

object FusionAtlasBuild extends Build {
  // Declare a project in the root directory of the build.
  lazy val root = Project("fusionatlas", file("."))
}
