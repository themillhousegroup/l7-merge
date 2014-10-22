package com.themillhousegroup.l7

import scala.xml.Elem
import java.io.File
import scala.collection.mutable.ListBuffer
import java.util.UUID
import com.typesafe.scalalogging.LazyLogging

/** Represents the tree structure of Layer7 XML snippets */
object HierarchyBuilder extends LazyLogging {
  val topLevelMarkerId = -5002

  val ignoredTypes = Seq("PrivateKey")

  def isSupported(doc: Elem) = {
    !ignoredTypes.exists(_ == doc.label)
  }

  /**
   * Returns a sequence of TopLevelNode instances, each of
   * which may have HierarchyNode children
   */
  def fromFiles(files: Seq[File]): Seq[TopLevelNode] = {

    val nodes = files.flatMap { f =>
      val doc = scala.xml.XML.loadFile(f)
      if (isSupported(doc)) {
        Some(MutableTreeNode(id(doc), folderId(doc), guid(doc), version(doc), name(doc), None, doc, f, ListBuffer()))
      } else {
        logger.warn(s"File $f has contents that cannot be handled.")
        None
      }
    }

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

  private[this] def id(doc: Elem): Int = {
    doc \@ "id" toInt
  }

  private[this] def folderId(doc: Elem): Option[Int] = {
    doc.label match {
      case "Folder" => Some(doc \@ "folderId" toInt)
      case "Service" => Some(doc \ "ServiceDetail" \@ "folderId" toInt)
      case "Policy" => Some(doc \ "PolicyDetail" \@ "folderId" toInt)
      case _ => None
    }
  }

  private[this] def guid(doc: Elem): Option[UUID] = {
    doc.label match {
      case "Policy" => Some(UUID.fromString(doc \ "PolicyDetail" \@ "guid"))
      case _ => None
    }
  }

  private[this] def version(doc: Elem): Int = {
    doc \@ "version" toInt
  }

  private[this] def name(doc: Elem): String = {
    doc.label match {
      case "Service" => (doc \ "ServiceDetail" \ "Name").head.text
      case "Policy" => (doc \ "PolicyDetail" \ "Name").head.text
      case _ => (doc \\ "Name").head.text
    }
  }

  // While we build up the hierarchy ...
  private case class MutableTreeNode(
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
}

trait HierarchyNode {
  val id: Int
  val folderId: Option[Int]
  val guid: Option[UUID]
  val version: Int
  val name: String
  val parent: Option[HierarchyNode]
  val content: Elem
  val source: File
  val children: Seq[HierarchyNode]
}

case class TopLevelNode(val id: Int, val guid: Option[UUID], val version: Int, val name: String, val content: Elem, source: File, val children: Seq[HierarchyNode])
    extends HierarchyNode {
  val folderId = Some(HierarchyBuilder.topLevelMarkerId)
  val parent = None
}