package si.menih.adventofcode
package Day7P1

import si.menih.adventofcode.lib.FileHelper

object Device:
  val file = raw"(\d+) ([\w\.])+".r

  def parse (input: Iterator[String]) =
    var pwd = "/"
    input.toList.foldLeft(Map[String, List[(String, Int)]]())((tree, line) => {
      line match {
        case s"$$ cd $dir" => {
          pwd = dir match {
            case ".." => {
              val i = pwd.lastIndexOf("/", pwd.length - 2)
              if (i == -1) "/" else pwd.substring(0, i + 1)
            }
            case "/" => "/"
            case _ => s"$pwd$dir/"
          }

          if (!tree.contains(pwd)) then
            tree + (pwd -> List())
          else tree
        }
        case s"dir $dir" => {
          val d = s"$pwd$dir/"
          tree + (d -> List())
        }
        case file(size, name) => {
          val dir = tree(pwd)
          tree + (pwd -> ((name, size.toInt) :: dir))
        }
        case _ => tree
      }
    })

  def sizes (tree: Map[String, List[(String, Int)]]): Map[String, Int] =
    tree.keys.toList.sorted.reverseIterator.foldLeft(Map[String, Int]())((map, key) => {
      // println(key)
      val size = tree(key).map(_._2).sum
      val children = map.filter((ck, cv) => {
        ck.startsWith(key) && ck.count(_ == '/') == key.count(_ == '/') + 1
      })

      val childSize = children.map(_._2).sum

      map + (key -> (size + childSize))
    })


@main def Day07(args: String*): Unit =
  val inputs = FileHelper.readInputLines("day07.txt")


  val tree = Device.parse(inputs);
  val sizes = Device.sizes(tree)

  val result = sizes.map(_._2).filter(_ < 100_000).sum

  println(s"Result: $result")


