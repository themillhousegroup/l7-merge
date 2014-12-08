package com.themillhousegroup.l7.xml

import org.specs2.mutable.Specification

class AttributeChangerSpec extends Specification {

  val sourceElem =
    <parent>
      <children size="3">
        <child id="first">Original First</child>
        <child id="second">Original Second</child>
        <child id="third">Original Third</child>
      </children>
      <name id="first">Original Name</name>
    </parent>

  "AttributeChanger" should {

    "be able to perform blanket replacement of attributes" in {

      val result = AttributeChanger.convert(sourceElem, None, "id", "foo")

      val ids = result \\ "@id"

      ids.filter(_.text == "foo") must haveSize(4)
    }

    "be able to perform targeted replacement of attributes" in {

      val result = AttributeChanger.convert(sourceElem, Some("child"), "id", "foo")

      val ids = result \\ "@id"

      ids.filter(_.text == "foo") must haveSize(3)
    }

    "be able to perform blanket replacement of attributes based on old value" in {

      val result = AttributeChanger.convert(sourceElem, None, "id", "first", "foo")

      val ids = result \\ "@id"

      ids.filter(_.text == "foo") must haveSize(2)
    }

    "be able to perform targeted replacement of attributes based on old value" in {

      val result = AttributeChanger.convert(sourceElem, Some("child"), "id", "first", "foo")

      val ids = result \\ "@id"

      ids.filter(_.text == "foo") must haveSize(1)
    }
  }
}
