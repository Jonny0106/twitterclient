import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val finagle = "com.twitter" %% "finagle-http" % "21.4.0"
  lazy val lemonlabs = "io.lemonlabs" %% "scala-uri" % "3.2.0"
  lazy val org = "commons-codec" % "commons-codec" % "1.9"
}
