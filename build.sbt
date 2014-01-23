name := "hetero graph community detection"

version := "0.999"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

scalacOptions ++= Seq("-feature", "-deprecation")

initialCommands in console := """
import common.Common._
import common.HFCommon._
import collection.mutable.{Set => MSet, Map => MMap, Seq => MSeq, Buffer}
"""
