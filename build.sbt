import SbtAdditions._
import sbtrelease.Utilities._
import sbtunidoc.Plugin.UnidocKeys._
import ReleaseTransformations._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

lazy val buildSettings = Seq(
  organization := "org.spire-math",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  scalacOptions --= "-Ywarn-unused-import".ifScala210.value.toSeq,
  scalacOptions --= "-Ywarn-value-discard".ifScala210.value.toSeq, // SI-8632: fails with @sp on Unit
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  scalaJSStage in Test := FastOptStage,
  fork := false,
  parallelExecution in Test := false
)

lazy val algebraSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val docSettings = Seq(
  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(coreJVM, stdJVM, lawsJVM),
  site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "api"),
  git.remoteRepo := "git@github.com:non/algebra.git"
)

lazy val aggregate = project.in(file("."))
  .settings(algebraSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(unidocSettings: _*)
  .settings(site.settings: _*)
  .settings(ghpages.settings: _*)
  .settings(docSettings: _*)
  .aggregate(coreJVM, ringJVM, latticeJVM, lawsJVM, macrosJVM, stdJVM)
  .dependsOn(coreJVM, ringJVM, latticeJVM, lawsJVM, macrosJVM, stdJVM)
  .aggregate(coreJS, ringJS, latticeJS, lawsJS, macrosJS, stdJS)
  .dependsOn(coreJS, ringJS, latticeJS, lawsJS, macrosJS, stdJS)

lazy val core = crossProject
  .crossType(CrossType.Pure)
  .settings(moduleName := "algebra")
  .settings(mimaDefaultSettings: _*)
  // TODO: update this to a published stable version, e.g. 0.4.0
  //.settings(previousArtifact := Some("org.spire-math" %% "algebra" % "0.3.1"))
  .settings(algebraSettings: _*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val ring = crossProject
  .crossType(CrossType.Pure)
  .dependsOn(core)
  .settings(moduleName := "algebra-ring")
  .settings(mimaDefaultSettings: _*)
  // TODO: update this to a published stable version, e.g. 0.4.0
  //.settings(previousArtifact := Some("org.spire-math" %% "algebra-ring" % "0.3.1"))
  .settings(algebraSettings: _*)

lazy val ringJVM = ring.jvm
lazy val ringJS = ring.js

lazy val lattice = crossProject
  .crossType(CrossType.Pure)
  .dependsOn(core, ring)
  .settings(moduleName := "algebra-lattice")
  .settings(mimaDefaultSettings: _*)
  // TODO: update this to a published stable version, e.g. 0.4.0
  //.settings(previousArtifact := Some("org.spire-math" %% "algebra" % "0.3.1"))
  .settings(algebraSettings: _*)

lazy val latticeJVM = lattice.jvm
lazy val latticeJS = lattice.js

lazy val std = crossProject
  .crossType(CrossType.Pure)
  .dependsOn(core, ring, lattice)
  .settings(moduleName := "algebra-std")
  .settings(algebraSettings: _*)
  .settings(sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen))

lazy val stdJVM = std.jvm
lazy val stdJS = std.js

lazy val laws = crossProject
  .dependsOn(core, ring, lattice, std, macros)
  .settings(moduleName := "algebra-laws")
  .settings(algebraSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % "1.12.4",
      "org.typelevel" %%% "discipline" % "0.4",
      "org.scalatest" %%% "scalatest" % "3.0.0-M7" % "test"
    )
  )

lazy val lawsJVM = laws.jvm 
lazy val lawsJS = laws.js

lazy val macros = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "algebra-macros")
  .settings(algebraSettings: _*)
  .settings(crossVersionSharedSources:_*)
  .settings(scalaMacroDependencies:_*)

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

lazy val publishSettings = Seq(
  homepage := Some(url("http://spire-math.org")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  autoAPIMappings := true,
  apiURL := Some(url("https://non.github.io/algebra/api/")),

  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo <<= (version).apply { v =>
    val nexus = "https://oss.sonatype.org/"

    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <scm>
      <url>git@github.com:non/algebra.git</url>
      <connection>scm:git:git@github.com:non/algebra.git</connection>
    </scm>
    <developers>
      <developer>
        <id>johnynek</id>
        <name>P. Oscar Boykin</name>
        <url>https://github.com/johnynek/</url>
      </developer>
      <developer>
        <id>avibryant</id>
        <name>Avi Bryant</name>
        <url>https://github.com/avibryant/</url>
      </developer>
      <developer>
        <id>non</id>
        <name>Erik Osheim</name>
        <url>http://github.com/non/</url>
      </developer>
      <developer>
        <id>tixxit</id>
        <name>Tom Switzer</name>
        <url>http://github.com/tixxit/</url>
      </developer>
    </developers>
  ),
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

addCommandAlias("validate", ";compile;test;unidoc")

// Base Build Settings

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

def crossVersionSharedSources() =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies ++= paradisePlugin.ifScala210.value.toSeq,
  libraryDependencies ++= quasiquotesLib.value.toSeq
)

addCommandAlias("gitSnapshots", ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

// For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

val paradisePlugin = ("org.scalamacros"  % "paradise"    % "2.1.0-M5").compilerPlugin cross CrossVersion.full
val quasiquotesLib = ("org.scalamacros" %% "quasiquotes" % "2.1.0-M5").ifScala210 // in scala-reflect for 2.11+
