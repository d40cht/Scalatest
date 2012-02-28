
import sbt._
import Keys._

object ScalatestBuild extends Build
{
    type Sett = Project.Setting[_]
    
    val standardSettings = Defaults.defaultSettings
    
    lazy val root = Project(
        id          = "Scalatest",
        base        = file("."),
        settings    = standardSettings ++ Seq[Sett](
            name                := "Scalatest",
            libraryDependencies ++= Seq(
                "org.scalatest" % "scalatest_2.9.1" % "1.6.1",
                "org.scalaz" % "scalaz-full_2.9.1" % "6.0.4",
                "com.github.jsuereth.scala-arm" % "scala-arm_2.9.1" % "1.0"
            )
        )
    )
    
    /*val deps
    name            := "ParticleFilters"

    seq(ProguardPlugin.proguardSettings :_*)

    version         := "1.0"

    scalaVersion    := "2.9.1"

    libraryDependencies += "org.scalatest" % "scalatest_2.9.1" % "1.6.1"

    libraryDependencies +=  "com.sun.jna" % "jna" % "3.0.9"

    libraryDependencies += "org.scala-tools.time" % "time_2.8.1" % "0.5"

    libraryDependencies += "org.apache.commons" % "commons-math" % "2.2"

    //libraryDependencies += "org.scalaquery" %% "scalaquery" % "0.9.0"

    libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.4"

    libraryDependencies += "com.h2database" % "h2" % "1.3.163"

    libraryDependencies += "commons-io" % "commons-io" % "2.0.1"*/
}

