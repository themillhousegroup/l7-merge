package com.themillhousegroup.l7.xml

import scala.xml._
import scala.xml.transform.{ RuleTransformer, RewriteRule }

/**
 * For any of these methods, supplying a None means "wildcard" - all things will "match"
 */
object AttributeChanger {

  // Returns true if the Option is None, or if it's Some and it matches
  private[this] def wildcardMatch[T](opt: Option[T], t: T): Boolean = {
    opt.isEmpty || opt.contains(t)
  }

  def rewriteRule(label: Option[String], attribName: String, oldValue: Option[String], newValue: String): RewriteRule = {
    new RewriteRule {

      def innerTransform(n: Node): Node = n match {
        case elem @ Elem(_, lbl, atts, _, child @ _*) if wildcardMatch(label, lbl) => {
          val maybeAttrib: Option[Seq[Node]] = Option(atts(attribName))
          maybeAttrib.filter(attribs => attribs.exists(n => wildcardMatch(oldValue, n.text))).map { a =>
            elem.asInstanceOf[Elem] % Attribute(None, attribName, Text(newValue), Null) copy (child = child map innerTransform)
          }.getOrElse(elem.asInstanceOf[Elem].copy(child = child map innerTransform))

        }
        case elem @ Elem(_, _, _, _, child @ _*) => elem.asInstanceOf[Elem].copy(child = child map innerTransform)
        case _ => n

      }

      override def transform(n: Node) = innerTransform(n)
    }
  }

  def convert(doc: Node, label: Option[String], attribName: String, newValue: String): Elem = {
    val rr = rewriteRule(label, attribName, None, newValue)
    new RuleTransformer(rr).transform(doc).head.asInstanceOf[Elem]
  }

  def convert(doc: Node, label: Option[String], attribName: String, oldValue: String, newValue: String): Elem = {
    val rr = rewriteRule(label, attribName, Some(oldValue), newValue)
    new RuleTransformer(rr).transform(doc).head.asInstanceOf[Elem]
  }
}
