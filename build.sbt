name := "scustoms"

version := "0.1"

scalaVersion := "2.13.7"
val AkkaVersion = "2.6.17"

resolvers += Resolver.JCenterRepository

libraryDependencies ++= Seq(
  "com.typesafe.akka"           %% "akka-actor-typed"         % AkkaVersion,
  "com.typesafe.akka"           %% "akka-protobuf-v3"         % AkkaVersion,
  "com.typesafe.akka"           %% "akka-stream-typed"         % AkkaVersion,
  "com.typesafe.akka"           %% "akka-stream"         % AkkaVersion,
  "com.typesafe.akka"           %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "net.katsstuff"               %% "ackcord"                  % "0.17.1",
  "com.github.david-bouyssie"   %  "sqlite4s_native0.4_2.13"  % "0.4.1",
  "de.gesundkrank.jskills"      %  "jskills"                  % "1.1"
)

//libraryDependencies += "net.katsstuff" %% "ackcord"                 % "0.17.1" //For high level API, includes all the other modules
//libraryDependencies += "net.katsstuff" %% "ackcord-core"            % "0.17.1" //Low level core API
//libraryDependencies += "net.katsstuff" %% "ackcord-commands"        % "0.17.1" //Low to mid level Commands API
//libraryDependencies += "net.katsstuff" %% "ackcord-lavaplayer-core" % "0.17.1" //Low level lavaplayer API

// https://mvnrepository.com/artifact/de.gesundkrank.jskills/jskills
//libraryDependencies += "de.gesundkrank.jskills" % "jskills" % "1.1"
//libraryDependencies += "com.github.david-bouyssie" % "sqlite4s" % "0.4.0"