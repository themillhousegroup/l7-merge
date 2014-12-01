package com.themillhousegroup.l7.xml

import org.specs2.mutable.Specification

class LayerSevenXMLHelperSpec extends Specification {

  val sourceElem =
    <parent>
      <children>
        <child id="first">Original First</child>
        <child id="second">Original Second</child>
        <child id="third">Original Third</child>
      </children>
      <name>Original Name</name>
    </parent>

  "LayerSevenXMLHelper" should {
    "Allow a node to be converted into an encoded node" in {

      val result = LayerSevenXMLHelper.encodeChildren(sourceElem)

      result.child.mkString must not contain ("<")
    }
  }
}
