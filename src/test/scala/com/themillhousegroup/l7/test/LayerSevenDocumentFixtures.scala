package com.themillhousegroup.l7.test

import scala.xml.XML
import com.themillhousegroup.l7.HierarchyBuilder
import java.io.File

/**
 * A couple of Layer-7-esque documents for testing with, in various formats
 */
object LayerSevenDocumentFixtures {

  object Locations {
    val serviceVersionFour = "src/test/resources/service-L7Version4.xml"
    val serviceVersionFive = "src/test/resources/service-L7Version5.xml"

    val policyVersionZero = "src/test/resources/policy-Version0.xml"
    val policyVersionOne = "src/test/resources/policy-Version1.xml"
  }

  object Files {
    lazy val serviceVersionFour = new File(Locations.serviceVersionFour)
    lazy val serviceVersionFive = new File(Locations.serviceVersionFive)

    lazy val policyVersionZero = new File(Locations.policyVersionZero)
    lazy val policyVersionOne = new File(Locations.policyVersionOne)
  }

  object Elems {
    lazy val serviceVersionFour = XML.loadFile(Files.serviceVersionFour)
    lazy val serviceVersionFive = XML.loadFile(Files.serviceVersionFive)

    lazy val policyVersionZero = XML.loadFile(Files.policyVersionZero)
    lazy val policyVersionOne = XML.loadFile(Files.policyVersionOne)
  }

  object HierarchyNodes {
    lazy val serviceVersionFour = HierarchyBuilder.fromFile(Files.serviceVersionFour).get
    lazy val serviceVersionFive = HierarchyBuilder.fromFile(Files.serviceVersionFive).get

    lazy val policyVersionZero = HierarchyBuilder.fromFile(Files.policyVersionZero).get
    lazy val policyVersionOne = HierarchyBuilder.fromFile(Files.policyVersionOne).get
  }
}
