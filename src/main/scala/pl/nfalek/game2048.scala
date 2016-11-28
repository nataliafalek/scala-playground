package pl.nfalek

import java.io.FileInputStream

import sun.audio.{AudioPlayer, AudioStream}

object game2048 {
  def main(args: Array[String]): Unit = {
    // open the sound file as a Java input stream
    val gongFile = "/home/nfalek/Muzyka/jingle_cats_mp3.AU"
    val in = new FileInputStream(gongFile)

    // create an audiostream from the inputstream
    val audioStream = new AudioStream(in)

    // play the audio clip with the audioplayer class
    AudioPlayer.player.start(audioStream)
    Thread.sleep(1000)
    println(
      """
        |_____   _______________________   _____________________  ___________________
        |___  | / /__    |__  __/___  _/   __  ____/__    |__   |/  /__  ____/_  ___/
        |__   |/ /__  /| |_  /   __  /     _  / __ __  /| |_  /|_/ /__  __/  _____ \
        |_  /|  / _  ___ |  /   __/ /      / /_/ / _  ___ |  /  / / _  /___  ____/ /
        |/_/ |_/  /_/  |_/_/    /___/      \____/  /_/  |_/_/  /_/  /_____/  /____/
        |

     """.stripMargin)
    Thread.sleep(2200)
    println(
      """
        | _______ _     _  ______ _____ _______ _______ _______ _______ _______      _    _ _______  ______ _______ _____  _____  __   _
        | |       |_____| |_____/   |   |______    |    |  |  | |_____| |______       \  /  |______ |_____/ |______   |   |     | | \  |
        | |_____  |     | |    \_ __|__ ______|    |    |  |  | |     | ______|        \/   |______ |    \_ ______| __|__ |_____| |  \_|
        |

      """.stripMargin)
    Thread.sleep(3000)
    println(
      """
        |          {_}
        |          / \
        |         /   \
        |        /_____\
        |      {`_______`}
        |       // . . \\
        |      (/(__7__)\)
        |      |'-' = `-'|
        |      |         |
        |      /\       /\
        |     /  '.   .'  \
        |    /_/   `"`   \_\
        |   {__}###[_]###{__}
        |   (_/\_________/\_)
        |       |___|___|
        |  nati  |--|--|
        |       (__)`(__)

      """.stripMargin)

    Thread.sleep(2000)

    println("Sterowane: asdw")

    play_game(Board.initializeBoard())
  }
  var score = 0

  def play_game(b: Board): Board = {
    var wynik = println("Twoj wynik to: " + score)
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
    }

    def play: Board = {
      if (hasAnySpaceLeft()) {
        Board(rows).insertFieldRandomly()
      } else {
        throw new GameOverException
      }
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
        score += a0 + a1
        List(0, a0 + a1, a2, a3)
      } else if (a0 == a1 && a0 != a2 && a2 == a3) {
        score += a0 + a1 + a3 + a3
        List(0, 0, a0 + a1, a2 + a3)
      } else if (a1 == a2 && a1 != a0 && a1 != a3) {
        score += a1 + a2
        List(0, a0, a1 + a2, a3)
      } else if (a2 == a3 && a2 != a1 && a0!=a1) {
        score += a2 + a3
        List(0, a0, a1, a2 + a3)
      } else if (a0 == a1 && a1 ==a2 && a2 != a3) {
        score += a1 + a2
        List(0, a0, a1 + a2, a3)
      } else if (a1==a2 && a2==a3 && a0 != a1) {
        score += a2 + a3
        List(0, a0, a1, a2 + a3)
      } else if (a0==a1 && a1==a2 && a2 == a3) {
        score += a0 + a1 + a2 + a3
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
