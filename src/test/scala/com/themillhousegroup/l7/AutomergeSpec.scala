package com.themillhousegroup.l7

import org.specs2.mutable.Specification

class AutomergeSpec extends Specification {

  "Automerge" should {

    "Reject empty dirs" in {
      val result = Automerge("src/test/resources/empty", "src/test/resources/empty").dryRun

      result must beAFailedTry
    }
  }
}
