package pl.nfalek

import java.io.FileInputStream

import sun.audio.{AudioPlayer, AudioStream}

object gameTowerHanoi {
  def main(args: Array[String]) {
    playGame(Towers.start)
  }

  def playGame(t: Towers): Towers = {
    println(t)
    if (t.youStillPlay(t)) {
      println("\n Twój ruch: \n")
      val lineFrom = scala.io.StdIn.readLine()
      val lineTo = scala.io.StdIn.readLine()
      val new_Towers = Towers(t.towers, lineFrom.toInt, lineTo.toInt).updatedTower()
      playGame(new_Towers)
    }
    else {
      println("Wygrałeś, zacznij od nowa")
      Towers.start
    }
  }

  case class Towers(towers: List[List[Int]], fromTower: Int, toTower: Int) {

    override def toString(): String = {
      val towers_map = Map(
        0 -> "  *  ",
        1 -> "      - ",
        2 -> "     ___  ",
        3 -> "    _____  ",
        4 -> "   _______ ",
        5 -> "  _________  ",
        6 -> " ___________ ",
        7 -> "_____________"
      )
      towers.map(a => a.map(aa => towers_map(aa)).mkString("\n")).mkString("\n\n")
    }

    def filterIfZero(oneTower: List[Int]): List[(Int, Int)] = {
      oneTower.zipWithIndex.filter { case (value, idx) => value == 0 }
    }

    def filterIfNotZero(oneTower: List[Int]): List[(Int, Int)] = {
      oneTower.zipWithIndex.filter { case (value, idx) => value != 0 }
    }

    def updatedTower(): Towers = {

      val a = filterIfNotZero(towers(fromTower))
      val b = filterIfZero(towers(toTower))
      val c = filterIfNotZero(towers(toTower))

      if (a.nonEmpty && b.nonEmpty && c.isEmpty) {
        val (valueHeadFromTower, indexHeadFromTower) = a.head
        val indexLastToTower = b.last._2
        Towers(towers.updated(fromTower, towers(fromTower).updated(indexHeadFromTower, 0)).updated(toTower, towers(toTower).updated(indexLastToTower, valueHeadFromTower)), fromTower, toTower)
      } else if (a.nonEmpty && b.nonEmpty && c.nonEmpty) {
        val (valueHeadFromTower, indexHeadFromTower) = a.head
        val indexLastToTower = b.last._2
        val valueLastToTower = c.last._1
        if (valueLastToTower > valueHeadFromTower) {
          Towers(towers.updated(fromTower, towers(fromTower).updated(indexHeadFromTower, 0)).updated(toTower, towers(toTower).updated(indexLastToTower, valueHeadFromTower)), fromTower, toTower)
        }
        else {
          println("Zły ruch")
          Towers(towers, fromTower, toTower)
        }
      } else {
        println("Zły ruch")
        Towers(towers, fromTower, toTower)
      }
    }

    def youStillPlay(t: Towers): Boolean = {
      t.towers.last != Towers.start.towers.head
    }

  }

  object Towers {
    println("Liczba krążków (max. 7): ")
    val krazki = scala.io.StdIn.readLine().toInt

    val start = Towers(List(List.range(1, krazki + 1), List.fill(krazki)(0), List.fill(krazki)(0)), 0, 0)
  }

}

