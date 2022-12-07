package si.menih.adventofcode
package Day5P2

import si.menih.adventofcode.lib.FileHelper
import scala.collection.mutable.Stack

object Crane:
  def parseStacks (stacksInput: Iterator[String]): IndexedSeq[Stack[Char]] =
    val s = stacksInput.map(_.grouped(4).map(_.mkString.trim).toIndexedSeq).toIndexedSeq.reverse
    val indexes = s.head.map(_.toInt)
    val crates = s.tail.map(_.map(_ match {
      case s"[$c]" => c.head
      case _ => ' '
    }))

    Range(0, indexes.length).map(i => {
      val s: Stack[Char] = Stack()
      for (crate <- crates if i < crate.length && crate(i) != ' ') {
        s.push(crate(i))
      }

      s
    })

  def runInstructions (stacks: IndexedSeq[Stack[Char]], instructions: Iterator[String]): IndexedSeq[Stack[Char]] =
    instructions.foreach(_ match {
      case s"move $num from $from to $to" => {
        val fromStack = stacks(from.toInt - 1)
        val toStack = stacks(to.toInt - 1)
        val count = num.toInt

        for (crate <- fromStack.take(count).reverse) {
          toStack.push(crate)
        }

        for (_ <- 0 until count) {
          fromStack.pop()
        }
      }
      case _ => ()
    })

    stacks

@main def Day05(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day05.txt")

  val (rows, instructions) = inputs.span(_ != "")

  val stacks = Crane.parseStacks(rows)
  val finalStacks = Crane.runInstructions(stacks, instructions)
  val result = finalStacks.map(_.pop).mkString

  println(s"Result: $result")
