
name := "scustoms"
version := "latest"

scalaVersion := "2.13.7"

resolvers += Resolver.JCenterRepository

libraryDependencies ++= Seq(
  "net.katsstuff"               %% "ackcord"                  % "0.17.1",
  "de.gesundkrank.jskills"      %  "jskills"                  % "1.1",
  "org.xerial"                  %  "sqlite-jdbc"              % "3.36.0.3",
  "com.typesafe.slick"          %  "slick_2.13"               % "3.3.3",
  "org.slf4j"                   %  "slf4j-api"                % "1.7.32",
  "org.slf4j"                   %  "slf4j-simple"             % "1.7.32"
)

scalacOptions ++= Seq("-deprecation", "-feature")
Compile / mainClass := Some("com.scustoms.Main")

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

import NativePackagerHelper._

Universal / mappings ++= contentOf("src/main/resources")
Docker / maintainer := "mdiasribeiro"
Docker / version := version.value

dockerBaseImage := "openjdk:11-jre-slim"
