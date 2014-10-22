package com.themillhousegroup.l7

import java.io.File

object SingleDocumentComparison {
  import HierarchyNode._
  def compare(left: HierarchyNode, right: HierarchyNode) = {
    val older = olderOf(left, right)
    val newer = newerOf(left, right)

    println(s"$newer is newer than $older")
  }
}

object SingleDocumentComparisonCommand extends Command("compare") {

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
      } yield (SingleDocumentComparison.compare(left, right))
    }
  }
}
