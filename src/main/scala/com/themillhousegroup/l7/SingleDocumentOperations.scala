package com.themillhousegroup.l7

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import com.themillhousegroup.l7.commands.Command

object SingleDocumentOperations extends LazyLogging {

  import HierarchyNode._
  import HierarchyBuilder._
  import com.themillhousegroup.l7.commands.SingleDocumentMergeCommand.Options._
  def compare(left: HierarchyNode, right: HierarchyNode) = {
    merge(left, right, None)
  }

  def merge(left: HierarchyNode, right: HierarchyNode, destination: Option[File] = None, options: Seq[String] = Nil) = {

    val older = if (options.contains(versionAware)) olderOf(left, right) else left
    val newer = if (options.contains(versionAware)) newerOf(left, right) else right

    logger.info(s"Contents of 'newer' file: ${newer.source.getAbsolutePath} will be merged into 'older' file: ${older.source.getAbsolutePath}")

    if (((newer.id == older.id) && (newer.guid == older.guid)) || options.contains(forceMerge)) {
      if (destination.isEmpty) { // i.e. dry run mode
        logger.info("Looks like change can be merged")
      } else {
        val merged = mergeTogether(older, newer, destination.get, options)
        //println(s"Merged: $merged")
        logger.debug(s"Merged and wrote the following to ${merged.source.getAbsolutePath}:\n${merged.content}")
      }
    } else {
      logger.error(s"Files seem to be referring to different things. Details follow (older, then newer):")
      logger.error(s"IDs:        ${older.id}\t${newer.id}")
      logger.error(s"GUIDs:      ${older.guid}\t${newer.guid}")
      logger.error(s"folderIDs:  ${older.folderId}\t${newer.folderId}")
    }

  }
}

