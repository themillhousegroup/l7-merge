package com.themillhousegroup.l7.commands

import java.io.File
import com.themillhousegroup.l7.{ SingleDocumentOperations, HierarchyBuilder }

object SingleDocumentMergeCommand extends Command("merge-one") {

  val expectedArgs = "[file1] [file2] to merge the contents of file2 into file1"

  def runWith(args: Seq[String]) = {
    if (args.size != 2) {
      println("Usage: Provide two filenames to be merged")
    } else {
      val leftFile = new File(args(0))
      val rightFile = new File(args(1))
      for {
        left <- HierarchyBuilder.fromFile(leftFile)
        right <- HierarchyBuilder.fromFile(rightFile)
      } yield (SingleDocumentOperations.merge(left, right, Some(leftFile)))
    }
  }
}
