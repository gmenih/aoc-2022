package si.menih.adventofcode
package Day8P2

import si.menih.adventofcode.lib.FileHelper

extension [T](c: Iterable[T])
  def countUntil(f: T => Boolean): Int =
    val index = c.toList.indexWhere(f)
    if (index == -1) c.size else index + 1

object TreeLineObserver:
  class Forest(val trees: List[List[Int]]):
    def width: Int = trees.head.size
    def height: Int = trees.size

    def get(x: Int, y: Int): Int = trees(y)(x)

    def isEdge(x: Int, y: Int): Boolean = x == 0 || y == 0 || x == width - 1 || y == height - 1

    def score(x: Int, y: Int): Int =
      val tree = get(x, y)

      if isEdge(x, y) then return 0

      val vn = Math.max(0, x - 1).to(0, -1).countUntil(v => get(v, y) >= tree)
      val hn = Math.max(0, y - 1).to(0, -1).countUntil(v => get(x, v) >= tree)
      val vp = (x + 1).until(width).countUntil(v => get(v, y) >= tree)
      val hp = (y + 1).until(height).countUntil(v => get(x, v) >= tree)

      vn * hn * vp * hp

  def parse(inputs: Iterator[String]): Forest =
    val data = inputs.map(_.map(_ - '0').toList).toList
    new Forest(data)

  def solve(forest: Forest) =
    for
      y <- 0 until forest.height
      x <- 0 until forest.width
    yield forest.score(x, y)

@main def Day08(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day08.txt")

  val grid = TreeLineObserver.parse(inputs)
  val sol = TreeLineObserver.solve(grid)

  println(sol.max)
