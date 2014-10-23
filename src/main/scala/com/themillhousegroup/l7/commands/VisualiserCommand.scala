package com.themillhousegroup.l7.commands

import java.io.File
import com.themillhousegroup.l7.{ HierarchyVisualiser, HierarchyBuilder, DirectoryHelper }

object VisualiserCommand extends Command("visualise") {

  val expectedArgs = "[directory] to visualise the contents of a directory"

  def runWith(args: Seq[String]) = {
    if (args.isEmpty) {
      println("Usage: Provide the directory of Layer7 XML files to visualise")
    } else {
      val targetDirName = args.head
      val targetDir = new File(targetDirName)
      val files = DirectoryHelper.xmlFilesIn(targetDir)
      val hierarchy = HierarchyBuilder.fromFiles(files.get.contents)
      println(HierarchyVisualiser.visualise(hierarchy))
    }
  }
}
