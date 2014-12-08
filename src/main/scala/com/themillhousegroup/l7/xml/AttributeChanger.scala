package com.themillhousegroup.l7.xml

import scala.xml._
import scala.xml.transform.{ RuleTransformer, RewriteRule }

object AttributeChanger {

  def convert(doc: Node, label: Option[String], attribName: String, newValue: String): Elem = {

    val rewrite = new RewriteRule {

      def innerTransform(n: Node): Node = n match {
        case elem @ Elem(_, lbl, atts, _, child @ _*) if label.isEmpty || label.contains(lbl) => {
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

  /** TODO: make this function take an Option[String] for oldValue, then make the above function call this one with None */
  def convert(doc: Node, label: Option[String], attribName: String, oldValue: String, newValue: String): Elem = {
    val rewrite = new RewriteRule {

      def innerTransform(n: Node): Node = n match {
        case elem @ Elem(_, label, atts, _, child @ _*) => {
          val maybeAttrib: Option[Seq[Node]] = Option(atts(attribName))
          maybeAttrib.filter(attribs => attribs.exists(_.text == oldValue)).map { a =>
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
