package si.menih.adventofcode
package Day3P1

import si.menih.adventofcode.lib.FileHelper

object Rucksack:
  def score(c: Char): Int =
    if c >= 'A' && c <= 'Z' then c - 'A' + 27
    else c - 'a' + 1

  def splitInHalf(input: String): (String, String) =
    val half = input.length / 2
    (input.take(half), input.drop(half))

  def solve(sack: String): Int =
    val (left, right) = splitInHalf(sack)
    val common = left.intersect(right).head
    score(common)

@main def Day03(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day03.txt")

  var result = for (line <- inputs) yield Rucksack.solve(line)
  val sum = result.sum

  println(s"Result: $sum")
