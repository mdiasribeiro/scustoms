
name := "scustoms"
version := "0.03"

scalaVersion := "2.13.7"
val AkkaVersion = "2.6.17"

resolvers += Resolver.JCenterRepository

libraryDependencies ++= Seq(
  "com.typesafe.akka"           %% "akka-actor-typed"         % AkkaVersion,
  "com.typesafe.akka"           %% "akka-protobuf-v3"         % AkkaVersion,
  "com.typesafe.akka"           %% "akka-stream-typed"        % AkkaVersion,
  "com.typesafe.akka"           %% "akka-stream"              % AkkaVersion,
  "com.typesafe.akka"           %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "net.katsstuff"               %% "ackcord"                  % "0.17.1",
  "de.gesundkrank.jskills"      %  "jskills"                  % "1.1",
  "org.xerial"                  %  "sqlite-jdbc"              % "3.36.0.3",
  "com.typesafe.slick"          %  "slick_2.13"               % "3.3.3"
)

Compile / mainClass := Some("com.scustoms.Main")

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

import NativePackagerHelper._
import scala.sys.process._

lazy val timestamp = "date --utc +%Y-%m-%dT%TZ".!!.trim

Universal / mappings ++= contentOf("src/main/resources")
Docker / maintainer := "mdiasribeiro"
Docker / version := version.value
//Docker / defaultLinuxInstallLocation := "/opt/docker"
//Docker / daemonUser := "daemon"
//Docker / daemonUserUid := None

dockerBaseImage := "openjdk:11-slim"
