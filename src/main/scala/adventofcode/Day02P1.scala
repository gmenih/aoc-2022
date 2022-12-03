package si.menih
package adventofcode
package Day2Part1

import si.menih.adventofcode.lib.FileHelper

object RPS:
  def play (you: Char, me: Char): Int =
    val (a, b) = (you - 'A' + 1, me - 'X' + 1)
    a - b match
      case 0 => b + 3
      case -1 | 2 => b + 6
      case _ => b

@main def Day02(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day02.txt")

  val result = for (line <- inputs) yield line match
    case s"$a $b" => RPS.play(a.charAt(0), b.charAt(0))
    case _ => 0

  println(s"Result: ${result.sum}")
