package com.themillhousegroup.l7

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import scala.util.{ Success, Failure, Try }
import Failures._

object Automerge {
  def apply(existingDirectoryName: String, newerDirectoryName: String) = {
    new Automerge(
      new File(existingDirectoryName),
      new File(newerDirectoryName)
    )
  }
}

class Automerge(val existingDir: File, val newerDir: File) extends LazyLogging {

  def dryRun = merge(true)

  import DirectoryHelper.xmlFilesIn

  def merge(dryRun: Boolean = false): Try[DifferenceSet[File]] = {
    val maybeExistingDir = xmlFilesIn(existingDir)
    val maybeNewerDir = xmlFilesIn(newerDir)

    for {
      existingDir <- maybeExistingDir
      newerDir <- maybeNewerDir
      result <- DirectoryDifferenceInspector.diff(existingDir, newerDir)
    } yield result
  }

}

object Failures {
  def failWith(msg: String) = Failure(new IllegalArgumentException(msg))
}

