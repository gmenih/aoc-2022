package si.menih.adventofcode
package Day1P2

import si.menih.adventofcode.lib.FileHelper

def partitionInputs(inputs: List[String]): List[List[Int]] =
  // println(inputs.toList)
  val (a, b) = inputs.span(_ != "")
  val rest = b.drop(1)

  if (rest.isEmpty) List(a.map(_.toInt))
  else a.map(_.toInt) :: partitionInputs(rest)

@main def Day01(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day01.txt")

  val partitioned = partitionInputs(inputs.toList)
  val summed = partitioned.map(_.sum).sorted.reverse.take(3)
  val max = summed.sum;

  println("Max Calories: " + max)
