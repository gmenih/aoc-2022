package si.menih.adventofcode
package Day11P1

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

  def draw(commands: Iterator[String]): Unit =
    interpret(commands)
      .grouped(40)
      .map { v =>
        v.zipWithIndex
          .foldLeft(List.fill(40)('.')) { (list, a) =>
            val (i, b) = a
            if (i - b).abs <= 1 then list.updated(b, '#') else list
          }
          .mkString
      }
      .foreach(println)

@main def Day10(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day10.txt")

  val result = CRT.draw(inputs)
