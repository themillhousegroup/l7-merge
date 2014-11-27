package com.themillhousegroup.l7.commands

import java.io.File
import com.themillhousegroup.l7.{ SingleDocumentOperations, HierarchyBuilder }

object SingleDocumentMergeCommand extends Command("merge-one") {

  val expectedArgs = "<file1> <file2> [--force] to merge the contents of file2 into file1"

  def runWith(args: Seq[String]) = {
    if (args.size < 2) {
      println("Usage: Provide two filenames to be merged")
    } else {
      val leftFile = new File(args(0))
      val rightFile = new File(args(1))

      val options = args.drop(2).map(_.substring(2)) // Cut off the '--' prefix on options

      for {
        left <- HierarchyBuilder.fromFile(leftFile)
        right <- HierarchyBuilder.fromFile(rightFile)
      } yield (SingleDocumentOperations.merge(left, right, Some(leftFile), options))
    }
  }
}
