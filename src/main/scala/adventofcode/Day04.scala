package si.menih.adventofcode
package Day4

import si.menih.adventofcode.lib.FileHelper

case class Pair(a: Int, b: Int)

object UnPair:
  val pattern = raw"(\d+)-(\d+),(\d+)-(\d+)".r

  def parse(input: String): Option[(Pair, Pair)] =
    input match
      case pattern(a, b, c, d) => Some(Pair(a.toInt, b.toInt), Pair(c.toInt, d.toInt))
      case _ => None

  def toRanges(pairs: (Pair, Pair)): (Range, Range) =
    (pairs._1.a to pairs._1.b, pairs._2.a to pairs._2.b)

  def solvePart1(pairs: (Pair, Pair)): Boolean =
    val (r1, r2) = toRanges(pairs)
    val intersection = r1.intersect(r2)
    intersection.size >= r1.size || intersection.size >= r2.size

  def solvePart2(pairs: (Pair, Pair)): Boolean =
    val (r1, r2) = toRanges(pairs)
    r1.intersect(r2).nonEmpty

@main def Day04(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day04.txt")

  val (p1, p2) = inputs.map(UnPair.parse).flatten.duplicate
  val part1 = p1.count(UnPair.solvePart1)
  val part2 = p2.count(UnPair.solvePart2)

  println(s"Part1: ${part1}")
  println(s"Part2: ${part2}")
