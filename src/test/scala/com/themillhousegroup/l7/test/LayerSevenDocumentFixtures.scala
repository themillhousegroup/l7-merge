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
  }

  object Files {
    lazy val serviceVersionFour = new File(Locations.serviceVersionFour)
    lazy val serviceVersionFive = new File(Locations.serviceVersionFive)
  }

  object Elems {
    lazy val serviceVersionFour = XML.loadFile(Files.serviceVersionFour)
    lazy val serviceVersionFive = XML.loadFile(Files.serviceVersionFive)
  }

  object HierarchyNodes {
    lazy val serviceVersionFour = HierarchyBuilder.fromFile(Files.serviceVersionFour).get
    lazy val serviceVersionFive = HierarchyBuilder.fromFile(Files.serviceVersionFive).get
  }
}
