package com.themillhousegroup.l7.commands

import java.io.File
import com.themillhousegroup.l7.{ SingleDocumentOperations, HierarchyBuilder }

object SingleDocumentComparisonCommand extends Command("compare-one") {

  val expectedArgs = "[file1] [file2] to compare the contents of two files"

  def runWith(args: Seq[String]) = {
    if (args.size != 2) {
      println("Usage: Provide two filenames to be compared")
    } else {
      val leftFile = new File(args(0))
      val rightFile = new File(args(1))
      for {
        left <- HierarchyBuilder.fromFile(leftFile)
        right <- HierarchyBuilder.fromFile(rightFile)
      } yield (SingleDocumentOperations.compare(left, right))
    }
  }
}
