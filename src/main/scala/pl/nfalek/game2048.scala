package pl.nfalek

object game2048 {
  def main(args: Array[String]) {
//    println(shiftRight2(List(2, 2, 4, 8)) == List(0, 4, 4, 8))
//    println(shiftRight2(List(2, 4, 4, 2)) == List(0, 2, 8, 2))
//    println(shiftRight2(List(2, 4, 2, 2)) == List(0, 2, 4, 4))
//    println(shiftRight2(List(8, 4, 4, 2)) == List(0, 8, 8, 2))
//    println(shiftRight2(List(0, 4, 4, 2)) == List(0, 0, 8, 2))
//    println(shiftRight2(List(0, 4, 4, 0)) == List(0, 0, 0, 8))
//    println(shiftRight2(List(4, 4, 0, 0)) == List(0, 0, 0, 8), shiftRight2(List(4, 4, 0, 0)))
//    println(shiftRight2(List(0, 0, 2, 2)) == List(0, 0, 0, 4))
//    println(shiftRight2(List(0, 2, 2, 2)) == List(0, 0, 2, 4))
//    println(shiftRight2(List(4, 2, 2, 2)) == List(0, 4, 2, 4))
//    println(shiftRight2(List(2, 2, 2, 4)) == List(0, 2, 4, 4))
//    println(shiftRight2(List(2, 2, 2, 0)) == List(0, 0, 2, 4))
//    println(shiftRight2(List(2, 2, 2, 2)) == List(0, 0, 4, 4))
//        println(matrix.shiftMatrixRight())
//        println(matrix.shiftMatrixDown())
//        println(matrix.shiftMatrixLeft())
//        println(matrix.shiftMatrixUp())
//        println(matrix.insertFieldRandomly())
//        println(Board.initializeBoard())

  }

  val matrix = Board(List(
    List(0,0,2,0),
    List(2,2,2,2),
    List(0,0,2,4),
    List(0,0,0,0)
  ))

//
//  def shiftRight2(row: List[Int]): List[Int] = {
//
//    val a0 = row(0)
//    val a1 = row(1)
//    val a2 = row(2)
//    val a3 = row(3)
//
//    if (a0 == a1 && a1 == a2 && a2 == a3) {
//      List(0, 0, a0 + a1, a2 + a3)
//    } else if (a0 == a1 && a2 == a3 && a0 != a2 && a2 != 0) {
//      List(0, 0, a0 + a1, a2 + a3)
//    } else if (a0 == a1 && a2 == a3 && a0 != a2 && a2 == 0) {
//      List(0, 0, 0, a0 + a1)
//    } else if (a0 == a1 && a2!=a1 && a2 != a3 && a3 != 0) {
//      List(0, a0 + a1, a2, a3)
//    } else if (a0 == a1 && a1 == a2 && a2 != a3 && a3 == 0) {
//      List(0, 0, a0 + a1, a2)
//    } else if (a0 == 0 && a1 == a2 && a2 == a3) {
//      List(0, 0, a1, a2 + a3)
//    } else if (a0 != 0 && a1 == a2 && a2 == a3) {
//      List(0, a0, a1, a2 + a3)
//    } else if (a0 == a3 && a1 == a2 && a1 == 0) {
//      List(0, 0, 0, a0 + a3)
//    } else if (a1 == a2 && a1 != 0 && a3 != 0) {
//      List(0, a0, a1 + a2, a3)
//    } else if (a3 == 0 && a0 != a3 && a1 == a2) {
//      List(0, 0, a0, a1 + a2)
//    } else if (a3 == 0 && a0 == a3 && a1 == a2) {
//      List(0, 0, 0, a1 + a2)
//    } else if (a0 == a1 && a1 == a2 && a2 != a3) {
//      List(0, a0, a1 + a2, a3)
//    } else if (a0 == a1 && a1 == a3 & a0 == 0) {
//      List(0, 0, 0, a2)
//    } else if (a0!=a1 && a2==a3){
//      List(0, a0, a1, a2 + a3)
//    } else {
//      row
//    }
//
//  }
  case class Board(rows: List[List[Int]]) {

    def shiftZero(row: List[Int]): List[Int] = {
      val listWithoutZero = row.filter(aa => aa!=0)
      val rowWithoutZeroSize = row.filter(aa => aa != 0).size
      if (rowWithoutZeroSize < row.size) {
        val listOfZeroSize = row.size - rowWithoutZeroSize
        List.fill(listOfZeroSize)(0) ++ listWithoutZero
      } else {
        row
      }
    }

    def shiftRight(row: List[Int]): List[Int] = {
      val row2 = shiftZero(row)
      val a0 = row2(0)
      val a1 = row2(1)
      val a2 = row2(2)
      val a3 = row2(3)

      if (a0 == a1 && a0 != a2 && a2 != a3) {
        List(0, a0 + a1, a2, a3)
      } else if (a0 == a1 && a0 != a2 && a2 == a3) {
        List(0, 0, a0 + a1, a2 + a3)
      } else if (a1 == a2 && a1 != a0 && a1 != a3) {
        List(0, a0, a1 + a2, a3)
      } else if (a2 == a3 && a2 != a1 && a0!=a1) {
        List(0, a0, a1, a2 + a3)
      } else if (a0 == a1 && a1 ==a2 && a2 != a3) {
        List(0, a0, a1 + a2, a3)
      } else if (a1==a2 && a2==a3 && a0 != a1) {
        List(0, a0, a1, a2 + a3)
      } else if (a0==a1 && a1==a2 && a2 == a3) {
        List (0, 0, a0 + a1, a2 + a3)
      } else {
        row2
      }

    }

    def shiftLeft(row: List[Int]): List[Int] = {
      shiftRight(row.reverse).reverse
    }

    def shiftMatrixRight(): Board = {
        Board(rows.map(aa => shiftRight(aa)))
    }

    def shiftMatrixLeft(): Board = {
        Board(rows.map(aa => shiftLeft(aa)))
    }

    def shiftMatrixUp(): Board = {
        Board(rows.transpose.map(aa => shiftLeft(aa)).transpose)
    }

    def shiftMatrixDown(): Board = {
        Board(rows.transpose.map(aa => shiftRight(aa)).transpose)
    }

    def insertFieldRandomly(): Board = {
      val random = scala.util.Random
      val random_number = (random.nextInt(3-1) + 1)*2
      val rowsWithIndex = rows.map(aa => aa.zipWithIndex)
      val matrixWithIndex = rowsWithIndex.zipWithIndex.map { case (rowWithIndex, rowIndex) =>
        rowWithIndex.filter( { case (value, idx) => value == 0 })
          .map { case (value, idx) => (rowIndex, idx)}
      }.flatten

      val random_place = random.nextInt(matrixWithIndex.size)
      val listWithIndex = matrixWithIndex(random_place)
      val random_column = listWithIndex._1
      val random_row = listWithIndex._2

      Board(rows.updated(random_column, rows(random_column).updated(random_row, random_number)))

    }
  }

  object Board {
    def initializeBoard(): Board = {
      val rows = List(List.fill(4)(0), List.fill(4)(0), List.fill(4)(0), List.fill(4)(0))
      Board(rows).insertFieldRandomly().insertFieldRandomly()
    }
  }

}
