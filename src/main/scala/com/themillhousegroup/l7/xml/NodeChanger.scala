package com.themillhousegroup.l7.xml

import scala.xml._
import scala.xml.transform.{ RuleTransformer, RewriteRule }

object NodeChanger {

  /** Generates a RewriteRule that will put newValue into anything matching the xPathExpression */
  def rewrite(xPathExpression: => Seq[Node], newValue: Elem): RewriteRule = {
    new RewriteRule {

      def innerTransform(n: Node): Node = n match {
        case elem @ Elem(_, _, _, _, child @ _*) if xPathExpression.contains(elem) => newValue
        case elem @ Elem(_, _, _, _, child @ _*) => elem.asInstanceOf[Elem].copy(child = child map innerTransform)
        case _ => n
      }

      override def transform(n: Node) = innerTransform(n)
    }
  }

  /** Actually perform a conversion on the Node */
  def convertNodeAt(doc: Node, xPathExpression: => Seq[Node], newValue: Elem): Elem = {

    val rewriteRule = rewrite(xPathExpression, newValue)

    new RuleTransformer(rewriteRule).transform(doc).head.asInstanceOf[Elem]
  }
}
