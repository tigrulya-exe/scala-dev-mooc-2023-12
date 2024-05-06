
scalaVersion := "2.13.8"

name := "scala-dev-mooc-2023-12"
organization := "ru.otus"
version := "1.0"

libraryDependencies += Dependencies.scalaTest
libraryDependencies ++= Dependencies.zio
libraryDependencies ++= Dependencies.zioConfig
libraryDependencies ++= Dependencies.cats
libraryDependencies ++= Dependencies.fs2
libraryDependencies ++= Dependencies.http4s
libraryDependencies ++= Dependencies.circe
libraryDependencies ++= Dependencies.akkaContainers
libraryDependencies += Dependencies.liquibase
libraryDependencies += Dependencies.postgres
libraryDependencies += Dependencies.logback
libraryDependencies ++= Dependencies.quill
libraryDependencies ++= Dependencies.testContainers
libraryDependencies += Dependencies.zioHttp

scalacOptions += "-Ymacro-annotations"
scalacOptions += "-Xlint"
