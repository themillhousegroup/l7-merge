package com.themillhousegroup.l7.commands

object OptionProcessing {
  def withOption[T](options: Seq[String], optionName: String)(ifNotSet: => T)(ifSet: => T): T = {
    options.find(optionName == _).fold(ifNotSet) { _ => ifSet }
  }

  def withOption[T](optionName: String)(ifNotSet: => T)(ifSet: => T)(implicit options: Seq[String]): T =
    withOption(options, optionName)(ifNotSet)(ifSet)
}

abstract class Command(val name: String) {

  val expectedArgs: String

  def runWith(args: Seq[String], options: Seq[String]): Unit

  val options: Map[String, String] = Map()
}
