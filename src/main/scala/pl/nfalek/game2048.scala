package pl.nfalek

object game2048 {
    def main(args: Array[String]): Unit = {
      play_game(Board.initializeBoard())
     // println(Console.BLUE + "2")
    // val colors_map = Map (
    //   0 -> (Console.YELLOW + "0"),
    //   2 -> (Console.BLUE + "2"),
    //   4 -> (Console.RED + "4")
     //)
      //println(List(0, 2, 4).map(aa => colors_map(aa)), List(2, 2, 4).map(aa => colors_map(aa)))
      //println(colors_map(0))
    }

    def play_game(b: Board): Board = {
     // val new_board = Board.initializeBoard()
      println(b)
      val line = scala.io.StdIn.readLine()
      val fucking_Board = if (line == "a") {
        play_game(b.shiftMatrixLeft().play)
      } else if (line == "d") {
        play_game(b.shiftMatrixRight().play)
      } else if (line == "w") {
        play_game(b.shiftMatrixUp().play)
      } else if (line == "s") {
        play_game(b.shiftMatrixDown().play)
      } else {
       play_game(b)
      }
      println(fucking_Board)
     fucking_Board
    }

  case class Board(rows: List[List[Int]]) {
    override def toString(): String = {
      val colors_map = Map (
        0 -> Console.YELLOW,
        2 -> Console.BLUE,
        4 -> Console.RED,
        8 -> Console.MAGENTA,
        16 -> Console.CYAN,
        32 -> Console.WHITE,
        64 -> Console.BLINK,
        128 -> Console.GREEN
      ).withDefaultValue("")
      rows.map(row => row.map(aa => colors_map(aa) + aa).mkString("\t")).mkString("\n")
      //println(colors_map(2))
      //println(colors_map(4))
      //aa.map(bb => colors_map(bb))
    }

    def play: Board = {
       if (hasAnySpaceLeft()) {
         Board(rows).insertFieldRandomly()
       } else {
         throw new GameOverException
       }
 //       throw new GameOverException
 //     }
//      throw new GameOverException
    }

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

    def hasAnySpaceLeft(): Boolean = {
      rows.flatten.filter(aa => aa == 0).size > 0
    }
  }

  object Board {
    def initializeBoard(): Board = {
      val rows = List(List.fill(4)(0), List.fill(4)(0), List.fill(4)(0), List.fill(4)(0))
      Board(rows).insertFieldRandomly().insertFieldRandomly()
    }
  }

  class GameOverException extends Exception
}
