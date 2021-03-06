package com.themillhousegroup.l7.xml

import java.io.{ FileWriter, StringWriter, File }
import scala.xml._
import java.util.UUID
import org.apache.commons.lang3.{ StringEscapeUtils, StringUtils }
import scala.Some
import scala.xml.parsing.ConstructingParser

/**
 * Helper object specifically for working with the Layer7 dialect of XML file
 */
object LayerSevenXMLHelper {

  val xmlPreamble = """<?xml version='1.0' encoding='UTF-8'?>"""
  val xmlPreambleDoubleQuoted = xmlPreamble.replace("'", "\"")

  def id(doc: Elem): Int = {
    doc \@ "id" toInt
  }

  def replaceId(newId: Int)(doc: Elem): Elem = {
    AttributeChanger.convert(doc, None, "id", newId.toString)
  }

  def folderId(doc: Elem): Option[Int] = {
    doc.label match {
      case "Folder" => optAttrib(doc, "folderId").map(_.toInt)
      case "Service" => optAttrib(doc \ "ServiceDetail", "folderId").map(_.toInt)
      case "Policy" => optAttrib(doc \ "PolicyDetail", "folderId").map(_.toInt)
      case _ => None
    }
  }

  def replaceFolderId(newFolderId: Option[Int])(doc: Elem): Elem = {
    newFolderId.map { f =>
      AttributeChanger.convert(doc, None, "folderId", f.toString)
    }.getOrElse(doc)
  }

  def guid(doc: Elem): Option[UUID] = {
    doc.label match {
      case "Policy" => Some(UUID.fromString(doc \ "PolicyDetail" \@ "guid"))
      case _ => None
    }
  }

  def replaceGuid(newGuid: Option[UUID])(doc: Elem): Elem = {
    newGuid.map { guid =>
      AttributeChanger.convert(doc, Some("PolicyDetail"), "guid", guid.toString)
    }.getOrElse(doc)
  }

  def replacePolicyGuid(doc: Elem, existingGuid: String, replacementGuid: String): Elem = {
    AttributeChanger.convert(doc, Some("PolicyGuid"), "stringValue", existingGuid, replacementGuid)
  }

  def version(doc: Elem): Int = {
    doc \@ "version" toInt
  }

  def replaceVersion(newVersion: Int)(doc: Elem): Elem = {
    AttributeChanger.convert(doc, None, "version", newVersion.toString)
  }

  def serviceDetailPolicyRevision(doc: Elem): Option[Int] = {
    val properties = (doc \\ "ServiceDetail" \\ "Properties" \\ "Property")
    val maybeRevisionProperty = thatHasAttributeValue(properties, "key", "policyRevision")
    maybeRevisionProperty.map(prop => (prop \\ "LongValue").head.text.toInt)
  }

  def replacePolicyRevision(maybeNewRevision: Option[Int])(doc: Elem): Elem = {
    maybeNewRevision.fold(doc) { newRevision =>
      val properties = (doc \\ "ServiceDetail" \\ "Properties" \\ "Property")
      val targetNode = (thatHasAttributeValue(properties, "key", "policyRevision").get \\ "LongValue").head.asInstanceOf[Elem]
      val newTarget = targetNode.copy(child = Seq(Text(newRevision.toString)))
      NodeChanger.convertNodeAt(doc, targetNode, newTarget)
    }
  }

  def resourceVersion(doc: Elem): Option[Int] = {
    val maybeFirstResource = (doc \\ "Resources" \\ "Resource").headOption
    maybeFirstResource.flatMap { firstResource =>
      optAttrib(firstResource, "version").map(_.toInt)
    }
  }

  def replaceResourceVersion(maybeNewVersion: Option[Int])(doc: Elem): Elem = {
    maybeNewVersion.fold(doc) { newVersion =>
      val firstResource = (doc \\ "Resources" \\ "Resource").head
      val newResource = AttributeChanger.convert(firstResource, Some("Resource"), "version", newVersion.toString)
      NodeChanger.convertNodeAt(doc, (doc \\ "Resources" \\ "Resource"), newResource)
    }
  }

  def thatHasAttributeValue(nodes: Seq[Node], attName: String, attValue: String): Option[Node] = {
    nodes.find { node =>
      attValue == node \@ attName
    }
  }

  def name(doc: Elem): String = {
    doc.label match {
      case "Service" => (doc \ "ServiceDetail" \ "Name").head.text
      case "Policy" => (doc \ "PolicyDetail" \ "Name").head.text
      case _ => (doc \\ "Name").head.text
    }
  }

  def optAttrib(doc: NodeSeq, attributeName: String): Option[String] = {
    (doc \ ("@" + attributeName)).theSeq.headOption.map(_.text)
  }

  /** Various hacks to make a standard XML document look L7-originated */
  def writeTo(f: File, doc: Elem): File = {

    val sr = new StringWriter()
    LayerSevenXMLWriter.write(sr, doc)
    val escaped = sr.toString
    val unescaped = escaped.replace("&quot;", "\"")

    val writer = new FileWriter(f)
    writer.write(unescaped)
    writer.close

    f
  }

  /**
   * The Layer7 "embeds" resources by encoding them into the text of a l7p:Resource.
   * This function pulls out this content as a regular Elem
   */
  def extractResource(resourceNode: Node): Elem = {
    val txt = resourceNode.text
    val unescaped = StringEscapeUtils.unescapeXml(txt)

    sourceToXml(scala.io.Source.fromString(unescaped))
  }

  def readFromFile(f: File): Elem = sourceToXml(scala.io.Source.fromFile(f))

  private def sourceToXml(source: scala.io.Source): Elem = {
    // Can't just use XML.loadString as it eats CDATA blocks;
    // http://blog.markfeeney.com/2011/03/scala-xml-gotchas.html
    ConstructingParser.fromSource(source, true).document.docElem.asInstanceOf[Elem]
  }

  /** Encodes all the children of this resource, returning a new version of resourceNode */
  def encodeChildren(resourceNode: Node): Elem = {
    val encodedChildren = resourceNode.nonEmptyChildren.map(child => Text(StringEscapeUtils.escapeXml(child.toString)))
    resourceNode.asInstanceOf[Elem].copy(child = encodedChildren)
  }

  /** Stuffs the (encoded) content Elem into the resourceNode, returning the new result */
  def encodeResource(resourceNode: Node, content: Elem): Elem = {
    val txt = content.toString
    resourceNode.asInstanceOf[Elem].copy(child = Seq(Text(xmlPreambleDoubleQuoted + "\n" + txt + "\n")))
  }
}
