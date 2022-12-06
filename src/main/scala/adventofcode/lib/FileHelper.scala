package si.menih.adventofcode.lib

import scala.io.Source

object FileHelper:

  def readInputLines (fileName: String): Iterator[String] =
    Source.fromFile(s"./src/inputs/$fileName").getLines

  def readInputInts: String => Iterator[Int] = readInputLines(_).map(_.toInt)

  def readInput (fileName: String): String =
    Source.fromFile(s"./src/inputs/$fileName").mkString
