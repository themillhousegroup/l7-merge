package com.themillhousegroup.l7

import org.specs2.mutable.Specification
import java.io.File
import scala.xml.{ Elem, Node, NodeSeq }

class StackSpec extends Specification {

  val theXml = <html>
                 >
                 <head><title>Title</title></head>
                 <body>
                   <p>First Para</p>
                   <div>First Div</div>
                   <p>Second Para</p>
                   <div>Second Div</div>
                 </body>
               </html>

  "Node Finding" should {

    "be able to find one thing" in {
      val results = theXml \\ "p"

      results must haveSize(2)
    }

    "be able to find multiple things" in {

      val searchedThings = Set("p", "div")

      val results = theXml.descendant_or_self.filter(node => searchedThings.contains(node.label))

      println(results)
      results must haveSize(4)
    }

    "with implicit" in {

      class ExtendedNode(n: Node) {

        def \\\(labels: Set[String]): NodeSeq = {
          n.descendant_or_self.filter(node => labels.contains(node.label))
        }
      }

      implicit def node2extendedNode(n: Node): ExtendedNode = new ExtendedNode(n)

      val results = theXml \\\ Set("p", "div")

      println(results)
      results must haveSize(4)
    }
  }

}
