package com.themillhousegroup.l7

import org.specs2.mutable.Specification
import java.io.File

class HierarchySpec extends Specification {

  def filesIn(subFolderName: String) = {
    val path = s"src/test/resources/$subFolderName"
    val tryDir = DirectoryHelper.xmlFilesIn(new File(path))
    tryDir.get.contents
  }

  "HierarchyBuilder" should {
    "Be able to identify a top-level node" in {
      val result = HierarchyBuilder.fromFiles(filesIn("one"))

      result must haveLength(1)

      result.head must beAnInstanceOf[TopLevelNode]
    }

    "Be able to identify a 2-level hierarchy of folders" in {
      val result = HierarchyBuilder.fromFiles(filesIn("two"))

      result must haveLength(1)

      val top = result.head

      top must beAnInstanceOf[TopLevelNode]

      top.children must not beEmpty

      val secondLevel = top.children.head

      secondLevel.name must beEqualTo("Second Level Folder")
    }

    "Be able to identify a 3-level hierarchy of folders" in {
      val result = HierarchyBuilder.fromFiles(filesIn("three"))

      result must haveLength(1)

      println(HierarchyVisualiser.visualise(result))

      val top = result.head

      top must beAnInstanceOf[TopLevelNode]

      top.children must not beEmpty

      val secondLevel = top.children.head

      secondLevel.children must not beEmpty

      val thirdLevel = secondLevel.children.head

      thirdLevel.id must beEqualTo(31)

    }
  }
}
