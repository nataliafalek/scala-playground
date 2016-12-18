package pl.nfalek

import java.io.FileInputStream

import sun.audio.{AudioPlayer, AudioStream}

object gameTowerHanoi {
  def main(args: Array[String]) {
    playGame(Towers.start)
  }

  def playGame(t: Towers): Towers = {
    var score = println("\nMoves: " + move + "\n")
    println(t)
    if (t.youStillPlay(t)) {
      println("\nFrom tower: ")
      val lineFrom = scala.io.StdIn.readLine()
      println("To tower: ")
      val lineTo = scala.io.StdIn.readLine()
      val new_Towers = Towers(t.towers, lineFrom.toInt -1, lineTo.toInt -1).updatedTower()
      move += 1
      playGame(new_Towers)

    }
    else {
      println("You win!")
      Towers.start
    }
  }

  var move = 0

  case class Towers(towers: List[List[Int]], fromTower: Int, toTower: Int) {

    override def toString(): String = {
      val towers_map = Map(
        0 -> "      *  ",
        1 -> "      - ",
        2 -> "     ___  ",
        3 -> "    _____  ",
        4 -> "   _______ ",
        5 -> "  _________  ",
        6 -> " ___________ ",
        7 -> "_____________"
      )
      towers.map(list => list.map(row => towers_map(row)).mkString("\n")).mkString("\n\n")
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
          println("Bad move! You can not put bigger disc on the smaller!")
          Towers(towers, fromTower, toTower)
        }
      } else {
        println("Bad move! Try again")
        Towers(towers, fromTower, toTower)
      }
    }

    def youStillPlay(t: Towers): Boolean = {
      t.towers.last != Towers.start.towers.head
    }
  }

  object Towers {
    println("Number of discs (3-7): ")
    val discs = scala.io.StdIn.readLine().toInt
    val start = if (discs < 3 || discs > 7) {
      println("Please enter the correct number")
      val newDiscs = scala.io.StdIn.readLine().toInt
      Towers(List(List.range(1, newDiscs + 1), List.fill(newDiscs)(0), List.fill(newDiscs)(0)), 0, 0)
    } else {
      Towers(List(List.range(1, discs + 1), List.fill(discs)(0), List.fill(discs)(0)), 0, 0)
    }
  }
}

