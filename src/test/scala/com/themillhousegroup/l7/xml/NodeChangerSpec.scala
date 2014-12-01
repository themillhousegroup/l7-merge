package com.themillhousegroup.l7.xml

import org.specs2.mutable.Specification

class NodeChangerSpec extends Specification {

  val sourceElem =
    <parent>
      <children>
        <child id="first">Original First</child>
        <child id="second">Original Second</child>
        <child id="third">Original Third</child>
      </children>
      <name>Original Name</name>
    </parent>

  "NodeChanger" should {
    "Allow a simple node to be replaced" in {

      val replacementNameString = "Replacement Name"
      val newName = <name>{ replacementNameString }</name>

      val result = NodeChanger.convertNodeAt(sourceElem, sourceElem \\ "name", newName)

      (result \\ "name").head.text must beEqualTo(replacementNameString)
    }

    "Allow a nested node to be replaced" in {

      val replacementChildText = "Replacement Child"
      val newChild = <child id="r">{ replacementChildText }</child>

      val result = NodeChanger.convertNodeAt(sourceElem, sourceElem \\ "child", newChild)

      (result \\ "child").size must beEqualTo(3)
      (result \\ "child").head.text must beEqualTo(replacementChildText)
      (result \\ "child").last \@ "id" must beEqualTo("r")
    }
  }
}
