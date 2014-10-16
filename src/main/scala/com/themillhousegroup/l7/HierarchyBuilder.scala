package com.themillhousegroup.l7

import scala.xml.Elem
import java.io.File
import scala.collection.mutable.ListBuffer

/** Represents the tree structure of Layer7 XML snippets */
object HierarchyBuilder {
  val topLevelMarkerId = -5002

  /**
   * Returns a sequence of TopLevelNode instances, each of
   * which may have HierarchyNode children
   */
  def fromFiles(files: Seq[File]): Seq[TopLevelNode] = {

    val nodes = files.map { f =>
      val doc = scala.xml.XML.loadFile(f)
      MutableTreeNode(id(doc), folderId(doc), name(doc), None, doc, ListBuffer())
    }

    val topLevelNodes = nodes.filter(_.folderId == topLevelMarkerId)

    def assembleTree(parents: Seq[MutableTreeNode], remaining: Seq[MutableTreeNode]): Unit = {
      val children = remaining.diff(parents)
      val parentIds = parents.map(_.id)

      val nextLevel = children.filter { n =>
        parentIds.contains(n.folderId)
      }

      nextLevel.map { n =>
        val parent = parents.find(_.id == n.folderId).get
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
    doc.label match {
      case "Folder" | "Service" | "Policy" => doc \@ "id" toInt
      case _ => 0
    }
  }

  private[this] def folderId(doc: Elem): Int = {
    doc.label match {
      case "Folder" => doc \@ "folderId" toInt
      case "Service" => doc \ "ServiceDetail" \@ "folderId" toInt
      case "Policy" => doc \ "PolicyDetail" \@ "folderId" toInt
      case _ => 0
    }
  }

  private[this] def name(doc: Elem): String = {
    doc.label match {
      case "Folder" => (doc \\ "Name").head.text
      case "Service" => (doc \ "ServiceDetail" \ "Name").head.text
      case "Policy" => (doc \ "PolicyDetail" \ "Name").head.text
      case _ => "???"
    }
  }

  // While we build up the hierarchy ...
  private case class MutableTreeNode(
      val id: Int, val folderId: Int, val name: String,
      parent: Option[HierarchyNode],
      val content: Elem,
      val children: ListBuffer[HierarchyNode]) extends HierarchyNode {

    def asTopLevelNode = TopLevelNode(id, name, content, children.toSeq)
  }
}

trait HierarchyNode {
  val id: Int
  val folderId: Int
  val name: String
  val parent: Option[HierarchyNode]
  val content: Elem
  val children: Seq[HierarchyNode]
}

case class TopLevelNode(val id: Int, val name: String, val content: Elem, val children: Seq[HierarchyNode])
    extends HierarchyNode {
  val folderId = HierarchyBuilder.topLevelMarkerId
  val parent = None
}