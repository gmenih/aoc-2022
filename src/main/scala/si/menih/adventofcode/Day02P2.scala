package si.menih
package adventofcode
package Day2Part2

import si.menih.adventofcode.lib.FileHelper

object RPS:
  def clamp (a: Int, b: Int): Int =
    1 + ((a + b) + 2) % 3

  def round (hand: Char, choice: Char): Int =
    val (a, b) = (hand - 'A' + 1, choice - 'X')
    b match
      case 0 => play(a, clamp(a, -1))
      case 1 => play(a, a)
      case 2 => play(a, clamp(a, 1))

  def play (a: Int, b: Int): Int =
    a - b match
      case 0 => b + 3
      case -1 | 2 => b + 6
      case _ => b

@main def Day02(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day02.txt")

  val result = for (line <- inputs) yield line match
    case s"$a $b" => RPS.round(a.charAt(0), b.charAt(0))
    case _ => 0

  println(s"Result: ${result.sum}")
