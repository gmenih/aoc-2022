package si.menih.adventofcode
package Day11P2

import si.menih.adventofcode.lib.FileHelper


def gcd(a: BigInt, b: BigInt): BigInt =
    if b == 0 then a else gcd(b, a % b)

def lcm(a: BigInt, b: BigInt): BigInt =
    a * b / gcd(a, b)

object MonkeyBusiness:
  type WorryFn = (v: BigInt) => BigInt
  type TestFn = (v: Int) => Boolean


  class Monkey(val id: Int, val items: List[BigInt], worryFn: WorryFn, val mod: Long, yes: Int, no: Int, val inspections: Long):

    def this () = this(0, List(), (v: BigInt) => v, 1, 0, 0, 0)

    def setId (id: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def clearItems (i: Int) = Monkey(id, List(), worryFn, mod, yes, no, inspections + i)
    def addItems (item: List[BigInt]) = Monkey(id, items ++ item, worryFn, mod, yes, no, inspections)
    def setWorry (worryFn: WorryFn) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def setMod (mod: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def setYes(yes: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)
    def setNo(no: Int) = Monkey(id, items, worryFn, mod, yes, no, inspections)

    def test (v: BigInt) = if v % mod == 0 then yes else no

    def processItems (commonMultiplier: BigInt): (List[(BigInt, Int)], Monkey) =
      val newItems = items.map(v => worryFn(v) % commonMultiplier).map(v => (v, test(v)))
      return (newItems, clearItems(newItems.size))


  def parse(inputs: Iterator[String]): Iterator[Monkey] =
    inputs.grouped(7).map(input => {
      input.map(_.trim).foldLeft(Monkey())((monkey, input) => {
        input match {
          case s"Monkey $s:" => monkey.setId(s.toInt)
          case s"Starting items: $s" => monkey.addItems(s.split(", ").map(_.toLong).map(BigInt.apply).toList)
          case s"Operation: new = old $op $s" => monkey.setWorry(s match {
            case "old" => (v: BigInt) => v * v
            case v => op match {
              case "*" => (v: BigInt) => v * s.toLong
              case "+" => (v: BigInt) => v + s.toLong
              case _ => (v: BigInt) => v
            }
          })
          case s"Test: divisible by $s" => monkey.setMod(s.toInt)
          case s"If true: throw to monkey $s" => monkey.setYes(s.toInt)
          case s"If false: throw to monkey $s" => monkey.setNo(s.toInt)
          case _ => monkey
        }
      })
    })

  def run (monkeys: IndexedSeq[Monkey], rounds: Int, commonMultiplier: BigInt): Long =
    Range(0, rounds).foldLeft(monkeys.toIndexedSeq)((ms, _) => {
      ms.foldLeft(ms)((list, m) => {
        // Me no likey
        val monkey = list(m.id)
        val (items, newMonkey) = monkey.processItems(commonMultiplier)

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

@main def Day11(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day11.txt")
  val monkeys = MonkeyBusiness.parse(inputs).toIndexedSeq


  val commonMultiplier = monkeys.foldLeft(BigInt(1))((p, m) => lcm(p, m.mod))

  val result = MonkeyBusiness.run(monkeys, 10_000, commonMultiplier);

  println(s"Result: $result")

