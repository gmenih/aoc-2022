package si.menih
package adventofcode
package Day6

import si.menih.adventofcode.lib.FileHelper
import scala.collection.mutable.Stack

object SignalDetector:
  def detect (input: String, distinct: Int) =
    distinct + input.sliding(distinct).map(v => v.distinct.length == distinct).indexOf(true)

@main def Day06(args: String*): Unit =
  val input = FileHelper.readInput("day06.txt")

  val part1 = SignalDetector.detect(input, 4)
  val part2 = SignalDetector.detect(input, 14)

  println(s"Part1: $part1")
  println(s"Part2: $part2")
