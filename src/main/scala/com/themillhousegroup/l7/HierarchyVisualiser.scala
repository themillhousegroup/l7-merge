package com.themillhousegroup.l7

import scala.StringBuilder
import org.apache.commons.lang3.StringUtils
import com.typesafe.scalalogging.LazyLogging
import java.io.File

object HierarchyVisualiser {

  val indentSize = 4

  def visualise(hierarchy: Seq[TopLevelNode]): String = {
    val sb = new StringBuilder()

    visualise(sb, hierarchy, 0)
    sb.toString
  }

  private def visualise(sb: StringBuilder, nodes: Seq[HierarchyNode], indentLevel: Int): Unit = {
    val sorted = nodes.sortWith { case (l, r) => l.name.compareTo(r.name) < 0 }

    sorted.foreach { n =>
      sb.append(spaces(indentLevel))
      sb.append(n.name)
      sb.append("\n")
      visualise(sb, n.children, indentLevel + indentSize)
    }
  }

  private def spaces(i: Int): String = {
    StringUtils.leftPad("", i)
  }
}

object VisualiserApp extends App {
  if (args.isEmpty) {
    println("Usage: Provide the directory of Layer7 XML files to visualise")
  } else {
    val targetDirName = args.head
    val targetDir = new File(targetDirName)
    val files = DirectoryHelper.xmlFilesIn(targetDir)
    val hierarchy = HierarchyBuilder.fromFiles(files.get.contents)
    println(HierarchyVisualiser.visualise(hierarchy))
  }
}
