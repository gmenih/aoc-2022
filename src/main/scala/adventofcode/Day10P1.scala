package si.menih.adventofcode
package Day10P1

import si.menih.adventofcode.lib.FileHelper

object CRT:
  def interpret(commands: Iterator[String]): IndexedSeq[Int] =
    commands.foldLeft(IndexedSeq[Int](1)) { (list, command) =>
      val last = if list.nonEmpty then list.last else 1
      command match
        case "noop" => list :+ last
        case s"addx $v" =>
          v.toIntOption match
            case Some(v) => list ++ List(last, last + v)
            case None => list
    }

@main def Day10(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day10.txt")

  val result = CRT.interpret(inputs)

  val indexes = List(20, 60, 100, 140, 180, 220)
  val solution = indexes.map(v => result(v - 1) * v).sum

  println(s"Solution: $solution")
