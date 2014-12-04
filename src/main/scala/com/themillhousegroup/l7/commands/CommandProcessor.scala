package com.themillhousegroup.l7.commands

import org.apache.commons.lang3.StringUtils

trait CommandProcessor {
  val knownCommands: Seq[Command]
  val optionPrefix = "--"
  val typoThreshold = 3

  def displayCommands = {
    println("Usage: Provide a command and optional args")
    println("Available commands are:\n")
    knownCommands.foreach { cmd =>
      println(s"${cmd.name} ${cmd.expectedArgs}")
    }
    println("\n")
  }

  def runCommand(args: Seq[String]) = {
    val desiredCommand = args.head
    knownCommands.find(desiredCommand == _.name).map { cmd =>
      runCommandWithArgs(cmd, args.tail)
    }.orElse {
      suggestCommands(desiredCommand)
      None
    }
  }

  private def runCommandWithArgs(cmd: Command, args: Seq[String]) = {
    val optionsAndArgs = args.partition(_.startsWith(optionPrefix))
    cmd.runWith(optionsAndArgs._2, optionsAndArgs._1)
  }

  private def suggestCommands(desiredCommand: String) = {
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
  }
}
