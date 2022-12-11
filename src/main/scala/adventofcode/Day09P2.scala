package si.menih.adventofcode
package Day9P2

import si.menih.adventofcode.lib.FileHelper

object RopePhysics:
  enum Direction { case Up, Down, Left, Right }; import Direction.*
  case class Point(x: Int, y: Int):
    def t = (x, y)

    def follow(p: Point): Point =
      val dx = p.x - x
      val dy = p.y - y
      if Math.abs(dx) > 1 || Math.abs(dy) > 1 then Point(x + dx.sign, y + dy.sign)
      else this

  val dMap = Map('U' -> Up, 'D' -> Down, 'L' -> Left, 'R' -> Right)

  case class Instruction(direction: Direction, distance: Int)

  class Rope(val head: Point, val tail: List[Point]):
    def followHead(): Rope =
      var prev = head
      var followedTail = tail.foldLeft(List[Point]()) { (list, knot) =>
        val newTail = knot.follow(prev)

        prev = newTail

        newTail :: list
      }

      Rope(head, followedTail)

    def move(instruction: Instruction): List[Rope] =
      val (dx, dy) = instruction.direction match
        case Up => (0, 1)
        case Down => (0, -1)
        case Left => (-1, 0)
        case Right => (1, 0)

      var prevHead = head
      var prevTail = tail
      List.fill(instruction.distance) {
        val (x, y) = prevHead.t

        val newHead = Point(x + dx, y + dy)
        val newTail = (prevHead :: prevTail).take(9)

        println(newTail)

        prevHead = newHead
        prevTail = newTail

        Rope(prevHead, prevTail).followHead()
      }

  def parse(inputs: Iterator[String]): Iterator[Option[Instruction]] = inputs.map { line =>
    line match
      case s"$dir $dist" => Some(Instruction(dMap(dir.head), dist.toInt))
      case _ => None
  }

  def steps(instruction: Iterator[Instruction]) =
    instruction.foldLeft(List(Rope(Point(0, 0), List()))) { (list, instruction) =>
      list ++ list.last.move(instruction)
    }

  def solve(ropePoints: List[Rope]): Int =
    val set = ropePoints.filter(_.tail.size == 9).map(_.tail.last).toSet
    println(set)

    set.size

@main def Day09(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day09.txt")

  val instructions = RopePhysics.parse(inputs)
  val ropePoints = RopePhysics.steps(instructions.flatten)
  val solution = RopePhysics.solve(ropePoints)

  println(s"Solution: $solution")
