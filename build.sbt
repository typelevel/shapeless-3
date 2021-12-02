import com.typesafe.tools.mima.core.{ProblemFilters, ReversedMissingMethodProblem}

val scala3Version = "3.1.0"

ThisBuild / organization := "org.typelevel"
ThisBuild / scalaVersion := scala3Version
ThisBuild / crossScalaVersions := Seq(scala3Version)
ThisBuild / mimaFailOnNoPrevious := false
ThisBuild / updateOptions := updateOptions.value.withLatestSnapshots(false)

val previousVersion = "3.0.0"

// GHA configuration

ThisBuild / githubWorkflowJavaVersions := List(JavaSpec.temurin("8"))
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

val JvmCond = s"matrix.platform == 'jvm'"
val JsCond = s"matrix.platform == 'js'"
val NativeCond = s"matrix.platform == 'native'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("validateJVM"), name = Some("Validate JVM"), cond = Some(JvmCond)),
  WorkflowStep.Sbt(List("validateJS"), name = Some("Validate JS"), cond = Some(JsCond)),
  WorkflowStep.Sbt(List("validateNative"), name = Some("Validate Native"), cond = Some(NativeCond)),
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.Equals(Ref.Branch("main")), RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "platform" -> List("jvm", "js", "native")

ThisBuild / githubWorkflowPublishPreamble +=
  WorkflowStep.Use(UseRef.Public("olafurpg", "setup-gpg", "v3"))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

val nativeSettings = Def.settings(
  libraryDependencies += "org.scala-native" %%% "junit-runtime" % nativeVersion % Test,
  addCompilerPlugin("org.scala-native" % "junit-plugin" % nativeVersion cross CrossVersion.full),
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v"),
)

// Aliases

addCommandAlias("validate", ";clean;validateJVM;validateJS;validateNative")
addCommandAlias("validateJVM", ";buildJVM;mimaJVM;testJVM")
addCommandAlias("validateJS", ";buildJS;mimaJS;testJS")
addCommandAlias("validateNative", ";buildNative;mimaNative;testNative")
addCommandAlias("buildJVM", ";derivingJVM/compile;testJVM/compile;typeableJVM/compile")
addCommandAlias("buildJS", ";derivingJS/compile;testJS/compile;typeableJS/compile")
addCommandAlias("buildNative", ";derivingNative/compile;testNative/compile")
addCommandAlias("mimaJVM", ";derivingJVM/mimaReportBinaryIssues;testJVM/mimaReportBinaryIssues;typeableJVM/mimaReportBinaryIssues")
addCommandAlias("mimaJS", ";derivingJS/mimaReportBinaryIssues;testJS/mimaReportBinaryIssues;typeableJS/mimaReportBinaryIssues")
addCommandAlias("mimaNative", ";derivingNative/mimaReportBinaryIssues;testNative/mimaReportBinaryIssues")
addCommandAlias("testJVM", ";derivingJVM/test;testJVM/test;typeableJVM/test")
addCommandAlias("testJS", ";derivingJS/test;testJS/test;typeableJS/test")
addCommandAlias("testNative", ";derivingNative/test;testNative/test")

// Projects

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(crossScalaVersions := Seq())
  .settings(noPublishSettings)
  .aggregate(
    derivingJVM,
    derivingJS,
    derivingNative,
    testJVM,
    testJS,
    testNative,
    typeableJVM,
    typeableJS
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
  .settings(mimaSettings)
  .settings(
     mimaBinaryIssueFilters ++= Seq(
       ProblemFilters.exclude[ReversedMissingMethodProblem]("shapeless3.deriving.internals.ErasedInstances.erasedMapK"),
       ProblemFilters.exclude[ReversedMissingMethodProblem]("shapeless3.deriving.internals.ErasedProductInstances.erasedProject"),
       ProblemFilters.exclude[ReversedMissingMethodProblem]("shapeless3.deriving.internals.ErasedProductInstances.erasedMapK")
     )
   )
  .settings(publishSettings)
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))

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
  .settings(mimaSettings)
  .settings(publishSettings)
  .nativeSettings(nativeSettings)
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))

lazy val testJVM = test.jvm
lazy val testJS = test.js
lazy val testNative = test.native

lazy val typeable = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/typeable"))
  .dependsOn(test % "test")
  .settings(
    moduleName := "shapeless3-typeable"
  )
  .settings(commonSettings)
  //.settings(mimaSettings) // Not yet
  .settings(publishSettings)
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))

lazy val typeableJVM = typeable.jvm
lazy val typeableJS = typeable.js

lazy val local = crossProject(JSPlatform, JVMPlatform)
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
  .settings(noPublishSettings)
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))

// Settings

lazy val commonSettings = Seq(
  crossScalaVersions := (ThisBuild / crossScalaVersions).value,

  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Yexplicit-nulls"
  ),
  Compile / doc / sources := Nil,

  libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test",
)

lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := Set("org.typelevel" %% moduleName.value % previousVersion),
  mimaBinaryIssueFilters := Seq()
)

lazy val publishSettings: Seq[Setting[_]] = Seq(
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/typelevel/shapeless-3")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/typelevel/shapeless-3"), "scm:git:git@github.com:typelevel/shapeless-3.git")),
  developers := List(
    Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr")),
    Developer("TimWSpence", "Tim Spence", "timothywspence@gmail.com", url("https://twitter.com/timwspence"))
  )
)

lazy val noPublishSettings =
  publish / skip := true
