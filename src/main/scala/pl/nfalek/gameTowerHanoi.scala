package pl.nfalek

import java.io.FileInputStream

import sun.audio.{AudioPlayer, AudioStream}

object gameTowerHanoi {
  def main(args: Array[String]) {
    println("Nati")
    val towerValue = Towers(List(List(1,2,3), List(0, 0 ,0), List(0, 0, 0)), 0, 1)
    println(towerValue.updatedTower())
  }

  case class Towers (towers: List[List[Int]], fromTower: Int, toTower: Int) {

    def filterIfZero (oneTower: List[Int]): List[(Int, Int)] = {
      oneTower.zipWithIndex.filter { case(value, idx) => value == 0 }
    }

    def filterIfNotZero (oneTower: List[Int]): List[(Int, Int)] = {
      oneTower.zipWithIndex.filter { case(value, idx) => value != 0 }
    }

    def updatedTower(): Towers = {

      val indexHeadFromTower = filterIfNotZero(towers(fromTower)).head._2
      val valueHeadFromTower = filterIfNotZero(towers(fromTower)).head._1
      val indexLastToTower = filterIfZero(towers(toTower)).last._2

      if (filterIfNotZero(towers(toTower)) != List()) {
        val valueLastToTower = filterIfNotZero(towers(toTower)).last._1
        if (valueLastToTower > valueHeadFromTower) {
          Towers(towers.updated(fromTower, towers(fromTower).updated(indexHeadFromTower, 0)).updated(toTower, towers(toTower).updated(indexLastToTower, valueHeadFromTower)), fromTower, toTower)
        } else {
          println("Zly ruch")
          Towers(towers, fromTower, toTower)
        }
      } else {
        Towers(towers.updated(fromTower, towers(fromTower).updated(indexHeadFromTower, 0)).updated(toTower, towers(toTower).updated(indexLastToTower, valueHeadFromTower)), fromTower, toTower)
      }
    }

    //  def youWin(): Boolean = {
    //    val indexLastTower = towers.zipWithIndex.map { case (v, i) => i}.last
    //  towers(indexLastTower) ==
    //  }


  }

//  Object Towers {
//    //  def startGame(): Towers = {
//    Towers(List(List(1, 2, 3), List(0, 0, 0), List(0, 0, 0)), 0, 0)
//    //   }
//  }

}

