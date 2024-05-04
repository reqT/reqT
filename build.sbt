ThisBuild / scalaVersion  := "2.12.17"
lazy val `reqT` = (project in file("."))
  .settings(
    name := "reqT",
    scalacOptions := Seq("-encoding", "utf8", "-deprecation", "-unchecked", "-Werror"),
  )