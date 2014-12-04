package com.themillhousegroup.l7.commands

abstract class Command(val name: String) {

  val expectedArgs: String

  def runWith(args: Seq[String], options: Seq[String]): Unit

  val options: Map[String, String] = Map()
}
