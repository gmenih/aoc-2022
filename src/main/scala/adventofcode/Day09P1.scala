package si.menih.adventofcode
package Day9P1

import si.menih.adventofcode.lib.FileHelper

object RopePhysics:
  enum Direction { case Up, Down, Left, Right }; import Direction.*
  case class Point(x: Int, y: Int):
    def isDiagonal (p: Point): Boolean =
      Math.abs(p.x - x) == Math.abs(p.y - y)

    def distance (p: Point): Int =
      Math.hypot(p.x - x, p.y - y).toInt

    def t = (x, y)

  val dMap = Map('U' -> Up, 'D' -> Down, 'L' -> Left, 'R' -> Right)

  case class Instruction(direction: Direction, distance: Int)

  class Rope(val head: Point, val tail: Option[Point]):
    def move (instruction: Instruction): List[Rope] =
      val (dx, dy) = instruction.direction match
        case Up => (0, 1)
        case Down => (0, -1)
        case Left => (-1, 0)
        case Right => (1, 0)

        var prevHead = head
        var prevTail = tail
        List.fill(instruction.distance){
          val (x, y) = prevHead.t
          val newHead = Point(x + dx, y + dy)

          val newTail =
            if prevTail.isEmpty || newHead.distance(prevTail.get) > 1 then prevHead
            else prevTail.get

          prevHead = newHead
          prevTail = Some(newTail)

          Rope(prevHead, prevTail)
        }

  def parse(inputs: Iterator[String]): Iterator[Option[Instruction]] = inputs.map { line =>
    line match
      case s"$dir $dist" => Some(Instruction(dMap(dir.head), dist.toInt))
      case _ => None
  }

  def steps (instruction: Iterator[Instruction]) =
    instruction.foldLeft(List(Rope(Point(0, 0), None)))((list, instruction) => {
      list ++ list.last.move(instruction)
    })

  def solve (ropePoints: List[Rope]): Int =
    ropePoints.map(v => v.tail).flatten.toSet.size

@main def Day09(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day09.txt")

  val instructions = RopePhysics.parse(inputs)
  val ropePoints = RopePhysics.steps(instructions.flatten)
  val solution = RopePhysics.solve(ropePoints)

  println(s"Solution: $solution")

