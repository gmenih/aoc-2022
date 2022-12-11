package si.menih.adventofcode
package Day8P1

import si.menih.adventofcode.lib.FileHelper

object TreeLineObserver:
  class Forest(val trees: List[List[Int]]):
    def width: Int = trees.head.size
    def height: Int = trees.size

    def get(x: Int, y: Int): Int = trees(y)(x)

    def isEdge(x: Int, y: Int): Boolean = x == 0 || y == 0 || x == width - 1 || y == height - 1

    def isVisible(x: Int, y: Int): Boolean =
      val tree = get(x, y)

      if isEdge(x, y) then return true
      else
        val vn = 0.until(x).forall(v => get(v, y) < tree)
        val hn = 0.until(y).forall(v => get(x, v) < tree)
        val vp = (x + 1).until(width).forall(v => get(v, y) < tree)
        val hp = (y + 1).until(height).forall(v => get(x, v) < tree)

        vn || hn || vp || hp

  def parse(inputs: Iterator[String]): Forest =
    val data = inputs.map(_.map(_ - '0').toList).toList
    new Forest(data)

  def solve(forest: Forest) =
    for
      y <- 0 until forest.height
      x <- 0 until forest.width
      if forest.isVisible(x, y)
    yield (x, y)

@main def Day08(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day08.txt")

  val grid = TreeLineObserver.parse(inputs)
  val sol = TreeLineObserver.solve(grid)

  println(sol.size)
