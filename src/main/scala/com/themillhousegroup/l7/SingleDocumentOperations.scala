package com.themillhousegroup.l7

import java.io.File

object SingleDocumentOperations {
  import HierarchyNode._
  def compare(left: HierarchyNode, right: HierarchyNode) = {
    val older = olderOf(left, right)
    val newer = newerOf(left, right)

    println(s"$newer is newer than $older")

    if ((newer.id == older.id)
      && (newer.guid == older.guid)
      && (newer.folderId == older.folderId)) {
      println("Looks like change can be merged")
    }
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
      } yield (SingleDocumentOperations.compare(left, right))
    }
  }
}
