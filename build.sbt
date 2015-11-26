lazy val root = (project in file(".")).
  settings(
    name := "hackerrank",
    scalaVersion := "2.11.7"
  )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2" % "test"
