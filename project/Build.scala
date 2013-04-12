import sbt._,Keys._

object Build extends Build {
  lazy val baseSettings = Seq(
    scalaVersion := "2.10.1",
    organization := "com.github.hexx",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:higherKinds"
    )
  )

  lazy val trs = Project(
    id = "trs",
    base = file(".")
  ).settings(
    baseSettings ++ seq(
      name := "trs",
      version := "0.0.1",
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.0.0-RC2"
      ),
      initialCommands in console += Seq(
        "scalaz._",
        "Scalaz._",
        "com.github.hexx.trs._"
      ).map("import " + _ + "\n").mkString
    ) : _*
  )
}
