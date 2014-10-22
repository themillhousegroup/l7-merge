package com.themillhousegroup.l7

import java.io.File

object SingleDocumentOperations {
  import HierarchyNode._
  import HierarchyBuilder._
  def compare(left: HierarchyNode, right: HierarchyNode) = {
    merge(left, right, None)
  }

  def merge(left: HierarchyNode, right: HierarchyNode, destination:Option[File] = None) = {
    val older = olderOf(left, right)
    val newer = newerOf(left, right)

    println(s"$newer is newer than $older")

    if ((newer.id == older.id)
      && (newer.guid == older.guid)
      && (newer.folderId == older.folderId)) {
      if (destination.isEmpty) { // i.e. dry run mode
        println("Looks like change can be merged")
      } else {
        val merged = mergeTogether(older, newer, destination.get)
      }
    }

  }
}

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

object SingleDocumentMergeCommand extends Command("merge-one") {

  val expectedArgs = "[file1] [file2] to merge the contents of the newer file into the older"

  def runWith(args: Seq[String]) = {
    if (args.size != 2) {
      println("Usage: Provide two filenames to be merged")
    } else {
      val leftFile = new File(args(0))
      val rightFile = new File(args(1))
      for {
        left <- HierarchyBuilder.fromFile(leftFile)
        right <- HierarchyBuilder.fromFile(rightFile)
      } yield (SingleDocumentOperations.merge(left, right))
    }
  }
}
