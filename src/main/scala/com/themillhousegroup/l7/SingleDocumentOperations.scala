package com.themillhousegroup.l7

import java.io.File
import com.typesafe.scalalogging.LazyLogging

object SingleDocumentOperations extends LazyLogging {
  import HierarchyNode._
  import HierarchyBuilder._
  def compare(left: HierarchyNode, right: HierarchyNode) = {
    merge(left, right, None)
  }

  def merge(left: HierarchyNode, right: HierarchyNode, destination: Option[File] = None) = {
    val older = olderOf(left, right)
    val newer = newerOf(left, right)

    logger.info(s"File: ${newer.source.getAbsolutePath}is newer than file: ${older.source.getAbsolutePath}")

    logger.debug(s"older:\n${older.content}\n\n")

    //    println(s"newer:\n${newer.content}\n\n")

    if ((newer.id == older.id)
      && (newer.guid == older.guid)) {
      if (destination.isEmpty) { // i.e. dry run mode
        logger.info("Looks like change can be merged")
      } else {
        val merged = mergeTogether(older, newer, destination.get)
        //println(s"Merged: $merged")
        logger.info(s"Merged and wrote the following to ${merged.source.getAbsolutePath}:\n${merged.content}")
      }
    } else {
      logger.error(s"Files seem to be referring to different things. Details follow (older, then newer):")
      logger.error(s"IDs:        ${older.id}\t${newer.id}")
      logger.error(s"GUIDs:      ${older.guid}\t${newer.guid}")
      logger.error(s"folderIDs:  ${older.folderId}\t${newer.folderId}")
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
