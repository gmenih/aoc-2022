package si.menih.adventofcode
package Day3P2

import si.menih.adventofcode.lib.FileHelper

object Rucksack:
  def score (c: Char): Int =
    if c >= 'A' && c <= 'Z' then
      c - 'A' + 27
    else
      c - 'a' + 1

  def solve (elves: Seq[String]): Int =
    elves match
      case Seq(a, b, c) =>
        val common = a.intersect(b).intersect(c).head
        score(common)
      case _ => 0

@main def Day03(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day03.txt")

  val result = for (lines <- inputs.grouped(3)) yield Rucksack.solve(lines)
  val sum = result.sum

  println(s"Result: $sum")

