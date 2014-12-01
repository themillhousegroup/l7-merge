package com.themillhousegroup.l7.commands

import java.io.File
import com.themillhousegroup.l7.{ SingleDocumentOperations, HierarchyBuilder }
import com.typesafe.scalalogging.LazyLogging

object SingleDocumentMergeCommand extends Command("merge-one") with LazyLogging {

  val expectedArgs =
    "<file1> <file2> [merge-options] to merge the contents of file2 into file1\n" +
      "  Where [merge-options] are:\n" +
      "    --force              Merge even if the files seem very different\n" +
      "    --only-structural    Retain the 'old' GUID references if any\n"
  val optionPrefix = "--"

  def runWith(args: Seq[String]) = {
    if (args.size < 2) {
      notEnoughFiles
    } else {
      val partition = args.partition(_.startsWith(optionPrefix))

      if (partition._2.size < 2) {
        notEnoughFiles
      } else {
        val leftFile = new File(partition._2(0))
        val rightFile = new File(partition._2(1))

        for {
          left <- HierarchyBuilder.fromFile(leftFile)
          right <- HierarchyBuilder.fromFile(rightFile)
        } yield (SingleDocumentOperations.merge(left, right, Some(leftFile), partition._1))
      }
    }
  }

  private[this] def notEnoughFiles = {
    logger.error("Usage: Provide two filenames to be merged")
  }
}
