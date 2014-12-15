package com.themillhousegroup.l7

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import com.themillhousegroup.l7.commands.SingleDocumentMergeCommand
import com.themillhousegroup.l7.commands.OptionProcessing.withOption
import scala.xml.Elem
import com.themillhousegroup.l7.xml.LayerSevenXMLHelper._
import com.themillhousegroup.l7.xml.NodeChanger
import scala.collection.mutable.ListBuffer

object SingleDocumentOperations extends LazyLogging {

  import HierarchyNode._
  import com.themillhousegroup.l7.commands.SingleDocumentMergeCommand.Options._
  def compare(left: HierarchyNode, right: HierarchyNode) = {
    merge(left, right, None)
  }

  def findOlderAndNewer(left: HierarchyNode, right: HierarchyNode, options: Seq[String] = Nil): (HierarchyNode, HierarchyNode) = {
    withOption(options, versionAware)(left -> right)(olderOf(left, right) -> newerOf(left, right))
  }

  /**
   * @return true if a merge was actually performed
   */
  def merge(left: HierarchyNode, right: HierarchyNode, destination: Option[File] = None, options: Seq[String] = Nil): Option[HierarchyNode] = {

    val (older, newer) = findOlderAndNewer(left, right, options)

    logger.info(s"Contents of 'newer' file: ${newer.source.getAbsolutePath} will be merged into 'older' file: ${older.source.getAbsolutePath}")

    if (((newer.id == older.id) && (newer.guid == older.guid)) || options.contains(forceMerge)) {
      if (destination.isEmpty) { // i.e. dry run mode
        logger.info("Looks like change can be merged")
        None
      } else {
        val merged = mergeTogether(older, newer, destination.get)(options)
        //println(s"Merged: $merged")
        logger.debug(s"Merged and wrote the following to ${merged.source.getAbsolutePath}:\n${merged.content}")
        Some(merged)
      }
    } else {
      logger.error(s"Files seem to be referring to different things. Details follow (older, then newer):")
      logger.error(s"IDs:        ${older.id}\t${newer.id}")
      logger.error(s"GUIDs:      ${older.guid}\t${newer.guid}")
      logger.error(s"folderIDs:  ${older.folderId}\t${newer.folderId}")
      None
    }

  }

  private def retainOldReferences(older: Elem, newer: Elem): Elem = {
    val olderResource = extractResource((older \\ "Resources" \\ "Resource").head)
    val olderIncludes = olderResource \\ "PolicyGuid"
    val newerResourceNode = (newer \\ "Resources" \\ "Resource").head
    val newerResource = extractResource(newerResourceNode)
    val newerIncludes = newerResource \\ "PolicyGuid"

    if (olderIncludes.size != newerIncludes.size) {
      throw new IllegalStateException(s"Can only perform a structural-only merge if the number of references is the same. Old: ${olderIncludes.size} != New: ${newerIncludes.size}")
    }

    var hybrid = newerResource

    olderIncludes.zip(newerIncludes).foreach {
      case (olderNode, newerNode) =>
        val oldGuid = olderNode \@ "stringValue"
        val newGuid = newerNode \@ "stringValue"
        hybrid = replacePolicyGuid(hybrid, newGuid, oldGuid)
    }

    NodeChanger.convertNodeAt(newer, (newer \\ "Resources" \\ "Resource"), encodeResource(newerResourceNode, hybrid))
  }

  /** Returns (version, revision, resourceVersion) triple */
  def desiredVersions(older: HierarchyNode, newer: HierarchyNode)(implicit options: Seq[String]): (Int, Int, Int) = {
    withOption(SingleDocumentMergeCommand.Options.retainOldVersions)(
      (newer.version, serviceDetailPolicyRevision(newer.content).get, resourceVersion(newer.content).get))(
        (older.version, serviceDetailPolicyRevision(older.content).get, resourceVersion(older.content).get))
  }

  def mergeTogether(older: HierarchyNode, newer: HierarchyNode, destinationFile: File)(implicit options: Seq[String] = Nil): HierarchyNode = {

    val innerContent = withOption(SingleDocumentMergeCommand.Options.onlyStructural)(newer.content)(retainOldReferences(older.content, newer.content))

    val (desiredVersion, desiredRevision, desiredResourceVersion) = desiredVersions(older, newer)

    val updatedContent =
      replaceId(
        replaceFolderId(
          replaceResourceVersion(
            replaceVersion(
              replacePolicyRevision(
                replaceGuid(innerContent, older.guid),
                desiredRevision),
              desiredVersion),
            desiredResourceVersion),
          older.folderId),
        older.id)

    writeTo(destinationFile, updatedContent)

    val newChildren = ListBuffer[HierarchyNode]()
    newChildren.insertAll(0, newer.children)

    val merged = MutableTreeNode(
      older.id,
      older.folderId,
      older.guid,
      desiredVersion,
      newer.name,
      older.parent,
      updatedContent,
      destinationFile,
      newChildren
    )

    merged
  }

}

