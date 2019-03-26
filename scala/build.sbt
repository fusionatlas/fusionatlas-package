name := "org.fusionatlas.scala"

organization := "fusionatlas.org"

version := "0.1.0"

scalaVersion := "2.11.7"

// we need local copies of all the jars for mathematica
retrieveManaged := true

resolvers ++= Seq(
	"Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
	"tqft.net Maven repository" at "https://tqft.net/releases",
	"Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	"Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases",
	"Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
	"Scala Tools Releases" at "http://scala-tools.org/repo-releases/",
	"ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-actors" % "2.11.7",
	"junit" % "junit" % "4.12",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
	"org.slf4j" % "slf4j-log4j12" % "1.7.12",
	"net.tqft" %% "toolkit-base" % "0.1.18-SNAPSHOT",
	"net.tqft" %% "toolkit-amazon" % "0.1.18-SNAPSHOT",
	"net.tqft" %% "toolkit-permutations" % "0.1.18-SNAPSHOT",
	"net.tqft" %% "toolkit-algebra" % "0.1.18-SNAPSHOT",
	"net.tqft" %% "toolkit-algebra-principalgraphs" % "0.1.18-SNAPSHOT",
	"net.tqft" %% "toolkit-algebra-experimental" % "0.1.18-SNAPSHOT",
	"net.java.dev.jets3t" % "jets3t" % "0.9.4",
	"com.google.code.typica" % "typica" % "1.7.2",
	"com.google.guava" % "guava" % "21.0",
	"org.apache.httpcomponents" % "httpcore" % "4.3.1",
	"org.apache.httpcomponents" % "httpclient" % "4.3.2"
)

publishTo := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa")))


