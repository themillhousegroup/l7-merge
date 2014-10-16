package com.themillhousegroup.l7

import org.specs2.mutable.Specification

class DifferenceInspectorSpec extends Specification {

  val di = new AbstractDifferenceInspector[String, String] {
    def identifier(thing: String): String = thing

    def diff(left: Seq[String], right: Seq[String]) = {
      calculateDifferenceSet(left, "left", right, "right")
    }
  }

  "AbstractDifferenceInspector" should {

    "Detect one added thing" in {
      val result = di.diff(Nil, Seq("one"))

      result must beASuccessfulTry

      val changes = result.get

      changes.added must haveLength(1)

      changes.added.head must beEqualTo("one")
    }

    "Detect multiple added things" in {
      val result = di.diff(Nil, Seq("one", "two", "three"))

      result must beASuccessfulTry

      val changes = result.get

      changes.added must haveLength(3)

      changes.added must beEqualTo(Seq("one", "two", "three"))
    }

    "Detect one removed thing" in {
      val result = di.diff(Seq("one"), Nil)

      result must beASuccessfulTry

      val changes = result.get

      changes.removed must haveLength(1)

      changes.removed.head must beEqualTo("one")
    }

    "Detect multiple removed things" in {
      val result = di.diff(Seq("one", "two", "three"), Nil)

      result must beASuccessfulTry

      val changes = result.get

      changes.removed must haveLength(3)

      changes.removed must beEqualTo(Seq("one", "two", "three"))
    }
  }
}
