package com.themillhousegroup.l7

import scala.xml._
import java.io.{ StringWriter, FileWriter, File }
import scala.collection.mutable.ListBuffer
import java.util.UUID
import com.typesafe.scalalogging.LazyLogging
import scala.xml.transform.{ RuleTransformer, RewriteRule }
import scala.Some

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

  /** Various hacks to make it look L7-originated */
  def writeTo(f: File, doc: Elem): File = {

    val sr = new StringWriter()
    XML.write(sr, doc, "UTF-8", true, null)
    val escaped = sr.toString
    val unescaped = escaped.replace("&quot;", "\"")
    val withStandalone = unescaped.replace(
      """<?xml version='1.0' encoding='UTF-8'?>""",
      """<?xml version="1.0" encoding="UTF-8" standalone="no"?>""")

    val writer = new FileWriter(f)
    writer.write(withStandalone)
    writer.close

    f

  }

  def mergeTogether(older: HierarchyNode, newer: HierarchyNode, destinationFile: File): HierarchyNode = {

    val updatedContent =
      replaceId(
        replaceFolderId(
          replaceGuid(newer.content, older.guid),
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

    println(updatedContent)

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

  def optAttrib(doc: NodeSeq, attributeName: String): Option[String] = {
    (doc \ ("@" + attributeName)).theSeq.headOption.map(_.text)
  }

  private[this] def id(doc: Elem): Int = {
    doc \@ "id" toInt
  }

  private[this] def replaceId(doc: Elem, newId: Int): Elem = {
    AttributeChanger.convert(doc, None, "id", newId.toString)
  }

  private[this] def folderId(doc: Elem): Option[Int] = {
    doc.label match {
      case "Folder" => optAttrib(doc, "folderId").map(_.toInt)
      case "Service" => optAttrib(doc \ "ServiceDetail", "folderId").map(_.toInt)
      case "Policy" => optAttrib(doc \ "PolicyDetail", "folderId").map(_.toInt)
      case _ => None
    }
  }

  private[this] def replaceFolderId(doc: Elem, newFolderId: Option[Int]): Elem = {
    newFolderId.map { f =>
      AttributeChanger.convert(doc, None, "folderId", f.toString)
    }.getOrElse(doc)
  }

  private[this] def guid(doc: Elem): Option[UUID] = {
    doc.label match {
      case "Policy" => Some(UUID.fromString(doc \ "PolicyDetail" \@ "guid"))
      case _ => None
    }
  }

  private[this] def replaceGuid(doc: Elem, newGuid: Option[UUID]): Elem = {
    newGuid.map { guid =>
      AttributeChanger.convert(doc, Some("PolicyDetail"), "guid", guid.toString)
    }.getOrElse(doc)
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
}

object AttributeChanger {

  def convert(doc: Node, label: Option[String], attribName: String, newValue: String): Elem = {
    val rewrite = new RewriteRule {

      def innerTransform(n: Node): Node = n match {
        case elem @ Elem(_, label, atts, _, child @ _*) => {
          val maybeAttrib = Option(atts(attribName))
          maybeAttrib.map { a =>
            elem.asInstanceOf[Elem] % Attribute(None, attribName, Text(newValue), Null) copy (child = child map innerTransform)
          }.getOrElse(elem.asInstanceOf[Elem].copy(child = child map innerTransform))

        }
        case elem @ Elem(_, _, _, _, child @ _*) => elem.asInstanceOf[Elem].copy(child = child map innerTransform)
        case _ => n

      }

      override def transform(n: Node) = innerTransform(n)
    }

    new RuleTransformer(rewrite).transform(doc).head.asInstanceOf[Elem]
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