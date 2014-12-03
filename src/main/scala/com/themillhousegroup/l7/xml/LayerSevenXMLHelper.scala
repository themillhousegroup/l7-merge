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

  def replaceId(doc: Elem, newId: Int): Elem = {
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

  def replaceFolderId(doc: Elem, newFolderId: Option[Int]): Elem = {
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

  def replaceGuid(doc: Elem, newGuid: Option[UUID]): Elem = {
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

  def replaceVersion(doc: Elem, newVersion: Int): Elem = {
    AttributeChanger.convert(doc, None, "version", newVersion.toString)
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
    XML.write(sr, doc, "UTF-8", true, null)
    val escaped = sr.toString
    val unescaped = escaped.replace("&quot;", "\"")
    val withStandalone = unescaped.replace(
      xmlPreamble,
      """<?xml version="1.0" encoding="UTF-8" standalone="no"?>""")
    val newlineStripped = withStandalone.replaceFirst("\\n", "")

    val writer = new FileWriter(f)
    writer.write(newlineStripped)
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

    // Can't just use XML.loadString as it eats CDATA blocks; http://blog.markfeeney.com/2011/03/scala-xml-gotchas.html
    ConstructingParser.fromSource(
      scala.io.Source.fromString(unescaped),
      true).document.docElem.asInstanceOf[Elem]
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
