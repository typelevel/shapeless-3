import com.typesafe.tools.mima.core.{ProblemFilters, ReversedMissingMethodProblem}

val scala3Version = "3.0.2"

ThisBuild / organization := "org.typelevel"
ThisBuild / scalaVersion := scala3Version
ThisBuild / crossScalaVersions := Seq(scala3Version)
ThisBuild / mimaFailOnNoPrevious := false
ThisBuild / updateOptions := updateOptions.value.withLatestSnapshots(false)

val previousVersion = "3.0.0"

// GHA configuration

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

val JvmCond = s"matrix.platform == 'jvm'"
val JsCond = s"matrix.platform == 'js'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("validateJVM"), name = Some("Validate JVM"), cond = Some(JvmCond)),
  WorkflowStep.Sbt(List("validateJS"), name = Some("Validate JS"), cond = Some(JsCond))
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.Equals(Ref.Branch("main")), RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "platform" -> List("jvm", "js")

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

// Aliases

addCommandAlias("validate", ";clean;validateJVM;validateJS")
addCommandAlias("validateJVM", ";buildJVM;mimaJVM;testJVM")
addCommandAlias("validateJS", ";buildJS;mimaJS;testJS")
addCommandAlias("buildJVM", ";derivingJVM/compile;testJVM/compile;typeableJVM/compile")
addCommandAlias("buildJS", ";derivingJS/compile;testJS/compile;typeableJS/compile")
addCommandAlias("mimaJVM", ";derivingJVM/mimaReportBinaryIssues;testJVM/mimaReportBinaryIssues;typeableJVM/mimaReportBinaryIssues")
addCommandAlias("mimaJS", ";derivingJS/mimaReportBinaryIssues;testJS/mimaReportBinaryIssues;typeableJS/mimaReportBinaryIssues")
addCommandAlias("testJVM", ";derivingJVM/test;testJVM/test;typeableJVM/test")
addCommandAlias("testJS", ";derivingJS/test;testJS/test;typeableJS/test")

// Projects

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(crossScalaVersions := Seq())
  .settings(noPublishSettings)
  .aggregate(
    derivingJVM,
    derivingJS,
    testJVM,
    testJS,
    typeableJVM,
    typeableJS
  )

lazy val deriving = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/deriving"))
  .dependsOn(test % "test")
  .settings(
    moduleName := "shapeless3-deriving",
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.6.1" % "test"
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

lazy val test = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/test"))
  .settings(
    moduleName := "shapeless3-test"
  )
  .settings(commonSettings)
  .settings(mimaSettings)
  .settings(publishSettings)
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))

lazy val testJVM = test.jvm
lazy val testJS = test.js

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

  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
)

lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := Set("org.typelevel" %% moduleName.value % previousVersion),
  mimaBinaryIssueFilters := Seq()
)

lazy val publishSettings: Seq[Setting[_]] = Seq(
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/milessabin/shapeless")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/shapeless"), "scm:git:git@github.com:milessabin/shapeless.git")),
  developers := List(
    Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
  )
)

lazy val noPublishSettings =
  publish / skip := true
