package com.themillhousegroup.l7

import com.themillhousegroup.l7.Directory
import java.io.File
import scala.util.{Success, Try}

object DirectoryHelper {
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
}