package com.themillhousegroup.l7

import org.specs2.mutable.Specification

class AutomergeSpec extends Specification {

  "Automerge" should {

    "Reject file handles that don't exist" in {
      val result = Automerge("src/test/resources/kjfh", "src/test/resources/one").dryRun

      result must beAFailedTry
    }

    "Reject file handles that aren't dirs" in {
      val result = Automerge("src/test/resources/one/level1.xml", "src/test/resources/one").dryRun

      result must beAFailedTry
    }

    "Reject empty dirs" in {
      val result = Automerge("src/test/resources/empty", "src/test/resources/empty").dryRun

      result must beAFailedTry
    }

    "Work, but do nothing if applied to the same dir" in {
      val result = Automerge("src/test/resources/two", "src/test/resources/two").dryRun

      result must beASuccessfulTry

      result.get.added must beEmpty
      result.get.removed must beEmpty
      result.get.modified must beEmpty
    }
  }
}
