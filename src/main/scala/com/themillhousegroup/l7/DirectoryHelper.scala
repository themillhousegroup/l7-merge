package com.themillhousegroup.l7

import java.io.File
import scala.util.{ Success, Try }
import Failures.failWith

case class Directory(dir: File, contents: Seq[File])

object DirectoryHelper {

  def filesIn(dir: File, pred: File => Boolean) = {
    if (!dir.exists) {
      failWith(s"Directory '${dir.getAbsolutePath}' does not exist. Cannot continue.")
    } else if (!dir.isDirectory) {
      failWith(s"File '${dir.getAbsolutePath}' is not a directory. Cannot continue.")
    } else {
      val xmlFiles: Seq[File] = dir.listFiles.filter(pred)
      if (xmlFiles.isEmpty) {
        failWith(s"Directory '${dir.getAbsolutePath}' is empty. Cannot continue.")
      } else {
        Success(Directory(dir, xmlFiles))
      }
    }
  }

  def xmlFilesIn(dir: File): Try[Directory] = filesIn(dir, _.getName.endsWith(".xml"))
}