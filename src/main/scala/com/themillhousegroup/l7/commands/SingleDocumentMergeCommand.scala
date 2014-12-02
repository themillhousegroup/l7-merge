package com.themillhousegroup.l7.commands

import java.io.File
import com.themillhousegroup.l7.{ SingleDocumentOperations, HierarchyBuilder }
import com.typesafe.scalalogging.LazyLogging

object SingleDocumentMergeCommand extends Command("merge-one") with LazyLogging {

  val expectedArgs =
    "<file1> <file2> [merge-options] to merge the contents of file2 into file1\n" +
      "  Where [merge-options] are:\n" +
      "    --force              Merge even if the files seem very different\n" +
      "    --only-structural    Retain the 'old' GUID references if any\n" +
      "    --version-aware      Use the version numbers in the files to work out newer vs older\n"

  def runWith(args: Seq[String], options: Seq[String]) = {
    if (args.size < 2) {
      notEnoughFiles
    } else {
      val leftFile = new File(args(0))
      val rightFile = new File(args(1))

      for {
        left <- HierarchyBuilder.fromFile(leftFile)
        right <- HierarchyBuilder.fromFile(rightFile)
      } yield (SingleDocumentOperations.merge(left, right, Some(leftFile), options))
    }
  }

  private[this] def notEnoughFiles = {
    logger.error("Usage: Provide two filenames to be merged")
  }
}
