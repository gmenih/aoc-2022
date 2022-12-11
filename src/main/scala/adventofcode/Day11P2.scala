package si.menih.adventofcode
package Day11P2

import si.menih.adventofcode.lib.FileHelper

// https://github.com/acmeism/RosettaCodeData/blob/master/Task/Least-common-multiple/Scala/least-common-multiple-1.scala
def gcd(a: Long, b: Long): Long = if b == 0 then a.abs else gcd(b, a % b)
def lcm(a: Long, b: Long) = (a * b).abs / gcd(a, b)

object MonkeyBusiness:
  type WorryFn = (v: Long) => Long
  type TestFn = (v: Int) => Boolean

  class Monkey(
      val id: Int,
      val items: List[Long],
      worryFn: WorryFn,
      val mod: Long,
      yes: Int,
      no: Int,
      val inspections: Long,
    ):
    def this() = this(0, List(), (v: Long) => v, 1, 0, 0, 0)

    def setId(id: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def clearItems(i: Int) = Monkey(id, List(), worryFn, mod, yes, no, inspections + i)
    def addItems(item: List[Long]) = Monkey(id, items ++ item, worryFn, mod, yes, no, inspections)
    def setWorry(worryFn: WorryFn) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def setMod(mod: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def setYes(yes: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def setNo(no: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)

    def test(v: Long) = if v % mod == 0 then yes else no

    def processItems(commonMultiple: Long): (List[(Long, Int)], Monkey) =
      val newItems = items.map(v => worryFn(v) % commonMultiple).map(v => (v, test(v)))
      return (newItems, clearItems(newItems.size))

  def parse(inputs: Iterator[String]): Iterator[Monkey] =
    inputs.grouped(7).map { input =>
      input.map(_.trim).foldLeft(Monkey()) { (monkey, input) =>
        input match
          case s"Monkey $s:" => monkey.setId(s.toInt)
          case s"Starting items: $s" => monkey.addItems(s.split(", ").map(_.toLong).toList)
          case s"Operation: new = old $op $s" =>
            monkey.setWorry(s match {
              case "old" => (v: Long) => v * v
              case v =>
                op match {
                  case "*" => (v: Long) => v * s.toLong
                  case "+" => (v: Long) => v + s.toLong
                  case _ => (v: Long) => v
                }
            })
          case s"Test: divisible by $s" => monkey.setMod(s.toInt)
          case s"If true: throw to monkey $s" => monkey.setYes(s.toInt)
          case s"If false: throw to monkey $s" => monkey.setNo(s.toInt)
          case _ => monkey
      }
    }

  def run(
      monkeys: IndexedSeq[Monkey],
      rounds: Int,
      commonMultiple: Long,
    ): Long =
    Range(0, rounds)
      .foldLeft(monkeys.toIndexedSeq) { (ms, _) =>
        ms.foldLeft(ms) { (list, m) =>
          // Me no likey
          val monkey = list(m.id)
          val (items, newMonkey) = monkey.processItems(commonMultiple)

          items.foldLeft(list.updated(monkey.id, newMonkey)) { (list2, item) =>
            val (worry, i) = item
            val monkeyAt = list2(i)
            val newMonkey = monkeyAt.addItems(List(worry))

            list2.updated(i, newMonkey)
          }
        }
      }
      .map(_.inspections)
      .sorted
      .reverse
      .take(2)
      .reduce(_ * _)

@main def Day11(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day11.txt")
  val monkeys = MonkeyBusiness.parse(inputs).toIndexedSeq

  val commonMultiple = monkeys.foldLeft(1L)((p, m) => lcm(p, m.mod))

  val result = MonkeyBusiness.run(monkeys, 10_000, commonMultiple);

  println(s"Result: $result")
