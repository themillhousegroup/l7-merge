package com.themillhousegroup.l7.xml

import scala.xml._
import scala.xml.Group
import scala.xml.Comment
import scala.xml.NamespaceBinding

/**
 * Knows how to write an `Elem` *just-so* to be minimally different from
 * a "natively-created" L7 XML file.
 */
object LayerSevenXMLWriter {

  def write(w: java.io.Writer,
    node: Node,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default) {

    w.write("""<?xml version="1.0" encoding="UTF-8" standalone="no"?>""")

    w.write(LayerSevenXMLWriter.serialize(node, minimizeTags = minimizeTags).toString)
  }

  /**
   * Ripped wholesale from scala.xml.Utility and bashed into shape.
   * Namely, put out elements in the form:
   *
   * <Elem xmlns:foo att1="bar" att2="baz"> ...
   *
   * for "compatibility" with Layer7 XML output.
   *
   * If we don't do this, Elems get serialized as:
   *
   * <Elem att2="baz" att1="bar" xmlns:foo> ...
   *
   * i.e. namespace at the end, and attribute-order flipped.
   */
  def serialize(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default): StringBuilder =
    {
      x match {
        case c: Comment if !stripComments => c buildString sb
        case s: SpecialNode => s buildString sb
        case g: Group =>
          for (c <- g.nodes) serialize(c, g.scope, sb, minimizeTags = minimizeTags); sb
        case el: Elem =>
          // print tag with namespace declarations
          sb.append('<')
          el.nameToString(sb)
          el.scope.buildString(sb, pscope)
          if (el.attributes ne null) flipAttribs(el).attributes.buildString(sb)

          if (el.child.isEmpty &&
            (minimizeTags == MinimizeMode.Always ||
              (minimizeTags == MinimizeMode.Default && el.minimizeEmpty))) {
            // no children, so use short form: <xyz .../>
            sb.append("/>")
          } else {
            // children, so use long form: <xyz ...>...</xyz>
            sb.append('>')
            sequenceToXML(el.child, el.scope, sb, stripComments)
            sb.append("</")
            el.nameToString(sb)
            sb.append('>')
          }
        case _ => throw new IllegalArgumentException("Don't know how to serialize a " + x.getClass.getName)
      }
    }

  /** Ripped from scala.xml.Utility and used by serialize for child nodes */
  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: MinimizeMode.Value = MinimizeMode.Default): Unit =
    {
      if (children.isEmpty) return
      else if (children forall isAtomAndNotText) { // add space
        val it = children.iterator
        val f = it.next()
        serialize(f, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
        while (it.hasNext) {
          val x = it.next()
          sb.append(' ')
          serialize(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
        }
      } else children foreach { serialize(_, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }
    }

  private[xml] def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]

  /**
   * By default, Scala's XML support reads attributes in 'reverse order' -
   * while it's not normally a problem, it is when we are trying to
   * minimise diffs when we write it back. So we reverse them here.
   */
  private def flipAttribs(e: Elem): Elem = {

    var nm = MetaData.normalize(Null, e.scope)
    e.attributes.toSeq.reverse.foreach { att =>
      att match {
        case md: MetaData => nm = nm.append(md, TopScope)
      }
    }
    e.copy(attributes = nm)
  }
}
