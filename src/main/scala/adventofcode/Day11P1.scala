package si.menih.adventofcode
package Day10P2

import si.menih.adventofcode.lib.FileHelper

object MonkeyBusiness:
  type WorryFn = (v: Int) => Int
  type TestFn = (v: Int) => Boolean
  class Monkey(val id: Int, val items: List[Int], worryFn: WorryFn, testFn: TestFn, yes: Int, no: Int, val inspections: Long):
    def this () = this(0, List(), (v: Int) => v, (v: Int) => true, 0, 0, 0)

    def setId (id: Int) = Monkey(id, items, worryFn, testFn, yes, no, inspections)
    def clearItems (i: Int) = Monkey(id, List(), worryFn, testFn, yes, no, inspections + i)
    def addItems (item: List[Int]) = Monkey(id, items ++ item, worryFn, testFn, yes, no, inspections)
    def setWorry (worryFn: WorryFn) = Monkey(id, items, worryFn, testFn, yes, no, inspections)
    def setTest (testFn: TestFn) = Monkey(id, items, worryFn, testFn, yes, no, inspections)
    def setYes(yes: Int) = Monkey(id, items, worryFn, testFn, yes, no, inspections)
    def setNo(no: Int) = Monkey(id, items, worryFn, testFn, yes, no, inspections)

    def test (v: Int) = if testFn(v) then yes else no

    def processItems (): (List[(Int, Int)], Monkey) =
      val newItems = items.map(v => worryFn(v) / 3).map(v => (v, test(v)))
      return (newItems, clearItems(newItems.size))

  def parse(inputs: Iterator[String]): Iterator[Monkey] =
    inputs.grouped(7).map(input => {
      input.map(_.trim).foldLeft(Monkey())((monkey, input) => {
        input match {
          case s"Monkey $s:" => monkey.setId(s.toInt)
          case s"Starting items: $s" => monkey.addItems(s.split(", ").map(_.toInt).toList)
          case s"Operation: new = old $op $s" => monkey.setWorry(op match {
            case "*" => (v: Int) => v * s.toIntOption.getOrElse(v)
            case "+" => (v: Int) => v + s.toIntOption.getOrElse(v)
            case "-" => (v: Int) => v - s.toIntOption.getOrElse(v)
            case "/" => (v: Int) => v / s.toIntOption.getOrElse(v)
            case _ => (v: Int) => v
          })
          case s"Test: divisible by $s" => monkey.setTest((v: Int) => v % s.toInt == 0)
          case s"If true: throw to monkey $s" => monkey.setYes(s.toInt)
          case s"If false: throw to monkey $s" => monkey.setNo(s.toInt)
          case _ => monkey
        }
      })
    })

  def run (monkeys: IndexedSeq[Monkey], rounds: Int): Long =
    Range(0, rounds).foldLeft(monkeys.toIndexedSeq)((ms, _) => {
      ms.foldLeft(ms)((list, m) => {
        // Me no likey
        val monkey = list(m.id)
        val (items, newMonkey) = monkey.processItems()

        items.foldLeft(list.updated(monkey.id, newMonkey))((list2, item) => {
          val (worry, i) = item
          val monkeyAt = list2(i)
          val newMonkey = monkeyAt.addItems(List(worry))

          list2.updated(i, newMonkey)
        })
    })
  })
    .map(_.inspections)
    .sorted
    .reverse
    .take(2)
    .reduce(_ * _)

@main def Day10(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day11.txt")

  val monkeys = MonkeyBusiness.parse(inputs).toIndexedSeq

  val result1 = MonkeyBusiness.run(monkeys, 20);
  val result2 = MonkeyBusiness.run(monkeys, 10_000);

  println(s"Part1: $result1")
  println(s"Part2: $result2")

