import com.typesafe.tools.mima.core.{ProblemFilters, ReversedMissingMethodProblem}

val scala3Version = "3.1.2"

ThisBuild / organization := "org.typelevel"
ThisBuild / tlBaseVersion := "3.1"
ThisBuild / scalaVersion := scala3Version
ThisBuild / crossScalaVersions := Seq(scala3Version)
ThisBuild / updateOptions := updateOptions.value.withLatestSnapshots(false)

// GHA configuration
ThisBuild / tlCiReleaseBranches := Seq("main")

val jsSettings = Def.settings(
  tlVersionIntroduced := Map("3" -> "3.0.1")
)

val nativeSettings = Def.settings(
  tlVersionIntroduced := Map("3" -> "3.1.0")
)

// Aliases

addCommandAlias("validate", ";clean;validateJVM;validateJS;validateNative")
addCommandAlias("validateJVM", ";buildJVM;mimaJVM;testJVM")
addCommandAlias("validateJS", ";buildJS;mimaJS;testJS")
addCommandAlias("validateNative", ";buildNative;mimaNative;testNative")
addCommandAlias("buildJVM", ";derivingJVM/compile;testJVM/compile;typeableJVM/compile")
addCommandAlias("buildJS", ";derivingJS/compile;testJS/compile;typeableJS/compile")
addCommandAlias("buildNative", ";derivingNative/compile;testNative/compile;typeableNative/compile")
addCommandAlias("mimaJVM", ";derivingJVM/mimaReportBinaryIssues;testJVM/mimaReportBinaryIssues;typeableJVM/mimaReportBinaryIssues")
addCommandAlias("mimaJS", ";derivingJS/mimaReportBinaryIssues;testJS/mimaReportBinaryIssues;typeableJS/mimaReportBinaryIssues")
addCommandAlias("mimaNative", ";derivingNative/mimaReportBinaryIssues;testNative/mimaReportBinaryIssues;typeableNative/mimaReportBinaryIssues")
addCommandAlias("testJVM", ";derivingJVM/test;testJVM/test;typeableJVM/test")
addCommandAlias("testJS", ";derivingJS/test;testJS/test;typeableJS/test")
addCommandAlias("testNative", ";derivingNative/test;testNative/test;typeableNative/test")

// Projects

lazy val root = tlCrossRootProject
  .aggregate(
    deriving,
    test,
    typeable
  )

lazy val deriving = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/deriving"))
  .dependsOn(test % "test")
  .settings(
    moduleName := "shapeless3-deriving",
  )
  .platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.7.0" % "test",
  )
  .jsSettings(jsSettings)
  .nativeSettings(
    nativeSettings,
    Test / sources := {
      // TODO enable if cats released
      val exclude = Set(
        "deriving.scala",
        "type-classes.scala",
        "adts.scala",
        "annotation.scala",
      )
      (Test / sources).value.filterNot { src =>
        exclude.contains(src.getName)
      }
    },
  )
  .settings(commonSettings)
  .settings(
     mimaBinaryIssueFilters ++= Seq(
       ProblemFilters.exclude[ReversedMissingMethodProblem]("shapeless3.deriving.internals.ErasedInstances.erasedMapK"),
       ProblemFilters.exclude[ReversedMissingMethodProblem]("shapeless3.deriving.internals.ErasedProductInstances.erasedProject"),
       ProblemFilters.exclude[ReversedMissingMethodProblem]("shapeless3.deriving.internals.ErasedProductInstances.erasedMapK")
     )
   )
  .jsEnablePlugins(ScalaJSJUnitPlugin)
  .nativeEnablePlugins(ScalaNativeJUnitPlugin)

lazy val derivingJVM = deriving.jvm
lazy val derivingJS = deriving.js
lazy val derivingNative = deriving.native

lazy val test = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/test"))
  .settings(
    moduleName := "shapeless3-test"
  )
  .settings(commonSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .jsEnablePlugins(ScalaJSJUnitPlugin)
  .nativeEnablePlugins(ScalaNativeJUnitPlugin)

lazy val testJVM = test.jvm
lazy val testJS = test.js
lazy val testNative = test.native

lazy val typeable = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/typeable"))
  .dependsOn(test % "test")
  .settings(
    moduleName := "shapeless3-typeable"
  )
  .settings(commonSettings)
  .settings(mimaPreviousArtifacts := Set.empty) // Not yet
  .nativeSettings(nativeSettings)
  .jsEnablePlugins(ScalaJSJUnitPlugin)
  .nativeEnablePlugins(ScalaNativeJUnitPlugin)

lazy val typeableJVM = typeable.jvm
lazy val typeableJS = typeable.js
lazy val typeableNative = typeable.native

lazy val local = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("local"))
  .dependsOn(deriving, test, typeable)
  .settings(
    moduleName := "shapeless3-local",
    scalacOptions ++= List("-Xmax-inlines", "1000"),
    scalacOptions += "-Xprint:postInlining",
    Compile / console / scalacOptions -= "-Xprint:postInlining",
    console / initialCommands := """import shapeless3.deriving.* ; import scala.deriving.*"""
  )
  .settings(commonSettings)
  .enablePlugins(NoPublishPlugin)
  .jsEnablePlugins(ScalaJSJUnitPlugin)
  .nativeEnablePlugins(ScalaNativeJUnitPlugin)

// Settings

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Yexplicit-nulls"
  ),
  Compile / doc / sources := Nil,
  libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v"),
)

ThisBuild / licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / developers := List(
  Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
  Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr")),
  Developer("TimWSpence", "Tim Spence", "timothywspence@gmail.com", url("https://twitter.com/timwspence"))
)
