import com.typesafe.sbt.SbtNativePackager._
import NativePackagerKeys._

name := "l7-merge"

version := s"${sys.props.getOrElse("build.majorMinor", "0.1")}.${sys.props.getOrElse("build.version", "SNAPSHOT")}"

scalaVersion := "2.11.2"

organization := "com.themillhousegroup"

libraryDependencies ++= Seq(
    "ch.qos.logback"              %   "logback-classic"       % "1.1.2",
    "com.typesafe.scala-logging"  %%  "scala-logging"         % "3.1.0",
    "org.scala-lang.modules"      %%  "scala-xml"             % "1.0.2",
    "org.apache.commons"          % "commons-lang3"           % "3.0",
    "org.mockito"                 %   "mockito-all"           % "1.9.0"       % "test",
    "org.specs2"                  %%  "specs2"                % "2.3.12"      % "test"
)

resolvers ++= Seq(  "oss-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                    "oss-releases"  at "https://oss.sonatype.org/content/repositories/releases",
                    "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

jacoco.settings

seq(bintraySettings:_*)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalariformSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

packageArchetype.java_application

