package com.themillhousegroup.l7

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import scala.util.{ Failure, Try }
import org.apache.commons.lang3.StringUtils
import com.themillhousegroup.l7.commands.{ VisualiserCommand, SingleDocumentComparisonCommand, SingleDocumentMergeCommand, Command }

object Automerge {
  def apply(existingDirectoryName: String, newerDirectoryName: String) = {
    new Automerge(
      new File(existingDirectoryName),
      new File(newerDirectoryName)
    )
  }
}

class Automerge(val existingDir: File, val newerDir: File) extends LazyLogging {

  def dryRun = merge(true)

  import DirectoryHelper.xmlFilesIn

  def merge(dryRun: Boolean = false): Try[DifferenceSet[File]] = {
    val maybeExistingDir = xmlFilesIn(existingDir)
    val maybeNewerDir = xmlFilesIn(newerDir)

    for {
      existingDir <- maybeExistingDir
      newerDir <- maybeNewerDir
      result <- DirectoryDifferenceInspector.diff(existingDir, newerDir)
    } yield result
  }

}

object Failures {
  def failWith(msg: String) = Failure(new IllegalArgumentException(msg))
}

object AutomergeApp extends App {

  private lazy val knownCommands = Seq[Command](
    SingleDocumentComparisonCommand,
    SingleDocumentMergeCommand,
    VisualiserCommand)

  private val typoThreshold = 3

  if (args.isEmpty) {
    println("Usage: Provide a command and optional args")
    println("Available commands are:\n")
    knownCommands.foreach { cmd =>
      println(s"${cmd.name} ${cmd.expectedArgs}")
    }
    println("\n")
  } else {
    val desiredCommand = args.head
    knownCommands.find(desiredCommand == _.name).map { cmd =>
      cmd.runWith(args.tail)
    }.orElse {
      val suggestions = knownCommands.filter { c =>
        val dist = StringUtils.getLevenshteinDistance(c.name, desiredCommand)
        dist < typoThreshold
      }
      if (suggestions.isEmpty) {
        println(s"Unknown command '$desiredCommand'")
      } else {
        println(s"Did you mean:")
        println(suggestions.map(_.name).mkString("  ", "\n", ""))
      }
      None
    }
  }
}

