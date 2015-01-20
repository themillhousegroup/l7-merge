package com.themillhousegroup.l7

import org.specs2.mutable.Specification
import com.themillhousegroup.l7.test.LayerSevenDocumentFixtures.HierarchyNodes._
import com.themillhousegroup.l7.commands.SingleDocumentMergeCommand
import java.io.File
import org.specs2.mock.Mockito
import scala.io.Source
import com.themillhousegroup.l7.test.LayerSevenDocumentFixtures

class SingleDocumentOperationsSpec extends Specification with Mockito {

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

      SingleDocumentOperations.compare(serviceVersionFive, serviceVersionFour) must beNone
    }

    "Support merging two service definitions" in {
      val dummyFile = File.createTempFile("tmp", ".tmp")
      val result = SingleDocumentOperations.merge(serviceVersionFive, serviceVersionFour, Some(dummyFile))

      result must beSome[HierarchyNode]

      println(result.get)

      result.get.version must beEqualTo(4)
    }

    "Create results that are indistinguishable from the originals for a service definition" in {
      val dummyFile = File.createTempFile("tmp", ".tmp")
      SingleDocumentOperations.merge(serviceVersionFour, serviceVersionFour, Some(dummyFile))

      val orig = Source.fromFile(LayerSevenDocumentFixtures.Files.serviceVersionFour).mkString
      val result = Source.fromFile(dummyFile).mkString

      result must beEqualTo(orig)
    }

    "Support merging two policy definitions" in {
      val dummyFile = File.createTempFile("tmp", ".tmp")
      val result = SingleDocumentOperations.merge(policyVersionZero, policyVersionOne, Some(dummyFile))

      result must beSome[HierarchyNode]

      println(result.get)

      result.get.version must beEqualTo(1)
    }

    "Create results that are indistinguishable from the originals for a policy definition" in {
      val dummyFile = File.createTempFile("tmp", ".tmp")
      SingleDocumentOperations.merge(policyVersionZero, policyVersionZero, Some(dummyFile))

      val orig = Source.fromFile(LayerSevenDocumentFixtures.Files.policyVersionZero).mkString
      val result = Source.fromFile(dummyFile).mkString

      result must beEqualTo(orig)
    }
  }
}
