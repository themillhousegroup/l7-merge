package com.themillhousegroup.l7

import org.specs2.mutable.Specification

class AutomergeSpec extends Specification {

  "Automerge" should {

    "Reject empty dirs" in {
      val result = Automerge("src/test/resources/empty", "src/test/resources/empty").dryRun

      result must beAFailedTry
    }

    "Detect new files" in {
      val result = Automerge("src/test/resources/old", "src/test/resources/new").dryRun

      result must beASuccessfulTry

      val changedFiles = result.get

      changedFiles must haveLength(1)

      changedFiles.head.getName must beEqualTo("level2.xml")
    }
  }
}
