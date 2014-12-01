package com.themillhousegroup.l7

import scala.xml._
import java.io.File
import scala.collection.mutable.ListBuffer
import java.util.UUID
import com.typesafe.scalalogging.LazyLogging
import scala.Some
import com.themillhousegroup.l7.xml.{ NodeChanger, AttributeChanger }
import com.themillhousegroup.l7.xml.LayerSevenXMLHelper._

/** Represents the tree structure of Layer7 XML snippets */
object HierarchyBuilder extends LazyLogging {
  val topLevelMarkerId = -5002

  val ignoredTypes = Seq("PrivateKey")

  def isSupported(doc: Elem) = {
    !ignoredTypes.exists(_ == doc.label)
  }

  /**
   * Builds a HierarchyNode based on what it finds in the file,
   * or None if it's of a type that is not supported.
   */
  def fromFile(file: File): Option[HierarchyNode] = {
    buildNode(file)
  }

  private def buildNode(f: File): Option[MutableTreeNode] = {
    val doc = scala.xml.XML.loadFile(f)
    if (isSupported(doc)) {
      Some(MutableTreeNode(id(doc), folderId(doc), guid(doc), version(doc), name(doc), None, doc, f, ListBuffer()))
    } else {
      logger.warn(s"File $f has contents that cannot be handled.")
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

    println(s"NeverRes:\n$newerResourceNode")

    var hybrid = newerResource

    olderIncludes.zip(newerIncludes).foreach {
      case (olderNode, newerNode) =>
        val oldGuid = olderNode \@ "stringValue"
        val newGuid = newerNode \@ "stringValue"
        hybrid = replacePolicyGuid(hybrid, newGuid, oldGuid)
    }

    val er = encodeResource(newerResourceNode, hybrid)

    println(er)

    NodeChanger.convertNodeAt(newer, (newer \\ "Resources" \\ "Resource"), er)
  }

  def mergeTogether(older: HierarchyNode, newer: HierarchyNode, destinationFile: File, options: Seq[String] = Nil): HierarchyNode = {

    val innerContent = options.find(SingleDocumentOperations.onlyStructural == _).fold(newer.content)(_ => retainOldReferences(older.content, newer.content))

    val updatedContent =
      replaceId(
        replaceFolderId(
          replaceGuid(innerContent, older.guid),
          older.folderId),
        older.id)

    writeTo(destinationFile, updatedContent)

    val newChildren = ListBuffer[HierarchyNode]()
    newChildren.insertAll(0, newer.children)

    val merged = MutableTreeNode(
      older.id,
      older.folderId,
      older.guid,
      newer.version,
      newer.name,
      older.parent,
      updatedContent,
      destinationFile,
      newChildren
    )

    //  println(updatedContent)

    merged
  }

  /**
   * Returns a sequence of TopLevelNode instances, each of
   * which may have HierarchyNode children
   */
  def fromFiles(files: Seq[File]): Seq[TopLevelNode] = {

    val nodes = files.flatMap(buildNode(_))

    val topLevelNodes = nodes.filter { n =>
      n.folderId.isEmpty || n.folderId.contains(topLevelMarkerId)
    }

    def assembleTree(parents: Seq[MutableTreeNode], remaining: Seq[MutableTreeNode]): Unit = {
      val children = remaining.diff(parents)
      val parentIds = parents.map(_.id)

      val nextLevel = children.filter { n =>
        parentIds.contains(n.folderId.get)
      }

      nextLevel.map { n =>
        val parent = parents.find(p => n.folderId.contains(p.id)).get
        parent.children += n
        n.copy(parent = Some(parent))
      }

      if (!children.isEmpty) {
        assembleTree(nextLevel, children)
      }
    }

    assembleTree(topLevelNodes, nodes)

    topLevelNodes.map(_.asTopLevelNode)
  }
}

object HierarchyNode {

  private def compareBy(f: => Boolean, a: HierarchyNode, b: HierarchyNode): HierarchyNode = {
    if (f) a else b
  }

  def olderOf(a: HierarchyNode, b: HierarchyNode) = {
    compareBy(a.version < b.version, a, b)
  }
  def newerOf(a: HierarchyNode, b: HierarchyNode) = {
    compareBy(a.version > b.version, a, b)
  }

}

trait HierarchyNode extends Product {
  val id: Int
  val folderId: Option[Int]
  val guid: Option[UUID]
  val version: Int
  val name: String
  val parent: Option[HierarchyNode]
  val content: Elem
  val source: File
  val children: Seq[HierarchyNode]

  override def toString = {
    s"$name ($id) v$version"
  }
}

// While we build up the hierarchy ...
private[l7] case class MutableTreeNode(
    val id: Int,
    val folderId: Option[Int],
    val guid: Option[UUID],
    val version: Int,
    val name: String,
    parent: Option[HierarchyNode],
    val content: Elem,
    val source: File,
    val children: ListBuffer[HierarchyNode]) extends HierarchyNode {

  def asTopLevelNode = TopLevelNode(id, guid, version, name, content, source, children.toSeq)
}

case class TopLevelNode(val id: Int, val guid: Option[UUID], val version: Int, val name: String, val content: Elem, source: File, val children: Seq[HierarchyNode])
    extends HierarchyNode {
  val folderId = Some(HierarchyBuilder.topLevelMarkerId)
  val parent = None
}