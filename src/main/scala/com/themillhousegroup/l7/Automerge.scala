package com.themillhousegroup.l7

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import scala.util.{ Success, Failure, Try }

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

  private[this] def failWith(msg: String) = Failure(new IllegalArgumentException(msg))

  def xmlFilesIn(dir: File): Try[Seq[File]] = {

    if (!dir.exists) {
      failWith(s"Directory '${dir.getAbsolutePath}' does not exist. Cannot continue.")
    } else if (!dir.isDirectory) {
      failWith(s"File '${dir.getAbsolutePath}' is not a directory. Cannot continue.")
    } else {
      val xmlFiles: Seq[File] = dir.listFiles.filter(_.getName.endsWith(".xml"))
      if (xmlFiles.isEmpty) {
        failWith(s"Directory '${dir.getAbsolutePath}' is empty. Cannot continue.")
      } else {
        Success(xmlFiles)
      }
    }
  }

  def merge(dryRun: Boolean = false): Try[Seq[File]] = {
    val maybeExistingFiles = xmlFilesIn(existingDir)
    val maybeNewerFiles = xmlFilesIn(existingDir)

    for {
      existingFiles <- maybeExistingFiles
      newerFiles <- maybeNewerFiles
      result <- process(existingFiles, newerFiles)
    } yield result
  }

  def process(existingFiles: Seq[File], newerFiles: Seq[File]): Try[Seq[File]] = {
    if (newerFiles.size < existingFiles.size) {
      failWith("I'm not clever enough to merge if #newer < #older. Sorry")
    } else if (newerFiles.size > existingFiles.size) {
      val diffs = existingFiles.diff(newerFiles)
      logger.info(s"The following files seem to be new in $newerDir:")
      logger.info(s"${diffs.map(_.getName).mkString(",")}")
      Success(diffs)
    } else {
      Success(Nil)
    }
  }
}
