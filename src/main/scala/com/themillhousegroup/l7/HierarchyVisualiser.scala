package com.themillhousegroup.l7

import scala.StringBuilder
import org.apache.commons.lang3.StringUtils
import com.typesafe.scalalogging.LazyLogging
import java.io.File
import com.themillhousegroup.l7.commands.Command

object HierarchyVisualiser {

  val indentSize = 4

  def visualise(hierarchy: Seq[TopLevelNode]): String = {
    val sb = new StringBuilder()

    visualise(sb, hierarchy, 0)
    sb.toString
  }

  private def visualise(sb: StringBuilder, nodes: Seq[HierarchyNode], indentLevel: Int): Unit = {
    val sorted = nodes.sortWith {
      case (l, r) => l.name.toLowerCase.compareTo(r.name.toLowerCase) < 0
    }

    sorted.foreach { n =>
      sb.append(spaces(indentLevel))
      sb.append(n)
      sb.append("\n")
      visualise(sb, n.children, indentLevel + indentSize)
    }
  }

  private def spaces(i: Int): String = {
    StringUtils.leftPad("", i)
  }
}

