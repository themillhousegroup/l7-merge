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

  def xmlFilesIn(dir: File): Try[Directory] = {

    if (!dir.exists) {
      failWith(s"Directory '${dir.getAbsolutePath}' does not exist. Cannot continue.")
    } else if (!dir.isDirectory) {
      failWith(s"File '${dir.getAbsolutePath}' is not a directory. Cannot continue.")
    } else {
      val xmlFiles: Seq[File] = dir.listFiles.filter(_.getName.endsWith(".xml"))
      if (xmlFiles.isEmpty) {
        failWith(s"Directory '${dir.getAbsolutePath}' is empty. Cannot continue.")
      } else {
        Success(Directory(dir, xmlFiles))
      }
    }
  }

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
