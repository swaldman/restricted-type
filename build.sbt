val nexus = "https://oss.sonatype.org/"
val nexusSnapshots = nexus + "content/repositories/snapshots";
val nexusReleases = nexus + "service/local/staging/deploy/maven2";

val coreProjectName = "restricted-type";

val commonSettings = Seq(
  organization := "com.mchange",
  version := "0.0.2-SNAPSHOT",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8"),
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  resolvers += ("releases" at nexusReleases),
  resolvers += ("snapshots" at nexusSnapshots),
  resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"),
  resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"),
  publishTo <<= version { 
    (v : String) => {
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexusSnapshots )
      else
        Some("releases"  at nexusReleases )
    }
  },
  pomExtra <<= name { 
    (projectName : String ) => (
      <url>https://github.com/swaldman/{projectName}</url>
      <licenses>
        <license>
        <name>GNU Lesser General Public License, Version 2.1</name>
        <url>http://www.gnu.org/licenses/lgpl-2.1.html</url>
          <distribution>repo</distribution>
        </license>
        <license>
        <name>Eclipse Public License, Version 1.0</name>
        <url>http://www.eclipse.org/org/documents/epl-v10.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:swaldman/{projectName}.git</url>
        <connection>scm:git:git@github.com:swaldman/{projectName}</connection>
      </scm>
      <developers>
        <developer>
          <id>swaldman</id>
          <name>Steve Waldman</name>
          <email>swaldman@mchange.com</email>
        </developer>
      </developers>
    )
  }
)

def makeSubproject( subname : String ) = {
  val projName = if ( subname == "core" ) coreProjectName else s"${coreProjectName}-${subname}";
  Project( projName, file( subname ) )
    .settings( commonSettings : _* )
}

lazy val core = makeSubproject( "core" )
  .settings(
    libraryDependencies ++= Seq(
      "com.mchange" %% "mchange-commons-scala" % "0.4.1",
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
    )
  )

lazy val scalacheckUtil = makeSubproject( "scalacheck-util" )
  .dependsOn( core )
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "compile"
    )
  )

// skip common settings and whatnot for this one 
// let the macro pick the variable name for name
lazy val root = project.in(file(".")).settings( publishArtifact := false, publish := {}, publishLocal := {} ).aggregate( core, scalacheckUtil );












