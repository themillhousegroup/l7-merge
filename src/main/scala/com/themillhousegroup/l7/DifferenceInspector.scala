package com.themillhousegroup.l7

import java.io.File
import scala.util.{ Success, Try }
import Failures._
import com.typesafe.scalalogging.LazyLogging

case class DifferenceSet[T](added: Seq[T] = Nil, removed: Seq[T] = Nil, modified: Seq[T] = Nil)

abstract class AbstractDifferenceInspector[T, I] extends LazyLogging {

  private def calculateDifference(larger: Seq[T], smaller: Seq[T]): Seq[T] = {
    val diffs = larger.map(identifier(_)).diff(smaller.map(identifier(_)))
    larger.filter(f => diffs.contains(identifier(f)))
  }

  protected def calculateDifferenceSet(existing: Seq[T], existingLabel: String, newer: Seq[T], newerLabel: String): Try[DifferenceSet[T]] = {
    println(s"existing $existing; newer: $newer")
    if (newer.size < existing.size) {
      val diffs = calculateDifference(existing, newer)
      logger.info(s"The following items seem to have been removed from $existingLabel:")
      logger.info(s"${diffs.mkString(",")}")
      Success(DifferenceSet(Nil, diffs))

    } else if (newer.size > existing.size) {
      val diffs = calculateDifference(newer, existing)
      logger.info(s"The following items seem to be new in $newerLabel:")
      logger.info(s"${diffs.mkString(",")}")
      Success(DifferenceSet(diffs))
    } else {

      // TODO - diffs *within* files
      Success(DifferenceSet())
    }
  }

  def identifier(thing: T): I
}

object DirectoryDifferenceInspector extends AbstractDifferenceInspector[File, String] {
  def identifier(dir: File): String = dir.getName

  def diff(existing: Directory, newer: Directory) = {
    calculateDifferenceSet(
      existing.contents,
      existing.dir.getName,
      newer.contents,
      newer.dir.getName
    )
  }
}
