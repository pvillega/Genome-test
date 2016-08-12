import sbt._

object Version {
  final val Scala     = "2.11.8"
  final val ScalaTest = "3.0.0"
  final val cats = "0.6.1"
}

object Library {
  val scalaTest = "org.scalatest" %% "scalatest" % Version.ScalaTest
  val cats = "org.typelevel" %% "cats" % Version.cats
}
