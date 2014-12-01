package com.themillhousegroup.l7.xml

import scala.xml._
import scala.xml.transform.{ RuleTransformer, RewriteRule }

object NodeChanger {

  def convertNodeAt(doc: Node, xPathExpression: => Seq[Node], newValue: Elem): Elem = {

    val rewrite = new RewriteRule {

      def innerTransform(n: Node): Node = n match {
        case elem @ Elem(_, _, _, _, child @ _*) if xPathExpression.contains(elem) => newValue
        case elem @ Elem(_, _, _, _, child @ _*) => elem.asInstanceOf[Elem].copy(child = child map innerTransform)
        case _ => n
      }

      override def transform(n: Node) = innerTransform(n)
    }

    new RuleTransformer(rewrite).transform(doc).head.asInstanceOf[Elem]
  }
}
