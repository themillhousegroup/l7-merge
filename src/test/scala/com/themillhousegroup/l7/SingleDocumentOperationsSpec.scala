package com.themillhousegroup.l7

import org.specs2.mutable.Specification
import com.themillhousegroup.l7.test.LayerSevenDocumentFixtures.HierarchyNodes._
import com.themillhousegroup.l7.commands.SingleDocumentMergeCommand

class SingleDocumentOperationsSpec extends Specification {

  "SingleDocumentOperations" should {
    "be able to find the newer version of two file if version-awareness wanted" in {

      val (older, newer) = SingleDocumentOperations.findOlderAndNewer(
        serviceVersionFive,
        serviceVersionFour,
        Seq(SingleDocumentMergeCommand.Options.versionAware))

      older.version must beLessThan(newer.version)
    }

    "retain original file order if version-awareness not wanted" in {

      val (older, newer) = SingleDocumentOperations.findOlderAndNewer(
        serviceVersionFive, serviceVersionFour)

      older.version must beGreaterThan(newer.version)
    }

    "Support comparing two things" in {

      SingleDocumentOperations.compare(serviceVersionFive, serviceVersionFour) must beFalse
    }
  }
}
