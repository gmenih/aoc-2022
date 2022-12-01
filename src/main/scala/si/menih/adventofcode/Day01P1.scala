package si.menih
package adventofcode
package Day1Part1

import si.menih.adventofcode.lib.FileHelper

def partitionInputs (inputs: Iterator[String]): List[List[Int]] =
  // println(inputs.toList)
  val (a, b) = inputs.span(_ != "")
  val rest = b.drop(1)

  if (rest.isEmpty) List(a.map(_.toInt).toList)
  else a.map(_.toInt).toList :: partitionInputs(rest)

@main def Day01Part1(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day01.txt")

  val partitioned = partitionInputs(inputs)
  val summed = partitioned.map(_.sum)
  val max = summed.max

  println(s"Max Calories: $max")
