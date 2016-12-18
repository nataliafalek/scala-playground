package pl.nfalek

import java.io.FileInputStream

import sun.audio.{AudioPlayer, AudioStream}

object game2048 {
  def main(args: Array[String]): Unit = {
    //favorite song
    val catMUsic = "/home/nfalek/Muzyka/jingle_cats_mp3.AU"
    val in = new FileInputStream(catMUsic)
    val audioStream = new AudioStream(in)
    AudioPlayer.player.start(audioStream)
    Thread.sleep(1000)
    //favorite effects
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
    println("Sterowanie: asdw")
    playGame(GameState(Board.initializeBoard(), true))
  }
  var score = 0

  //recursion function which shows your score and checks if you can move, next shifts a board in the right direction and inserts random number
  def playGame(b: GameState): GameState = {
    var wynik = println("Twoj wynik to: " + score)
    println(b.gameBoard)
    val line = scala.io.StdIn.readLine()
    if (b.canPlay) {
      val fuckingBoard = if (line == "a") {
        playGame(GameState(b.gameBoard.shiftMatrixLeft(), b.canPlay).gameBoard.play)
      } else if (line == "d") {
        playGame(GameState(b.gameBoard.shiftMatrixRight(), b.canPlay).gameBoard.play)
      } else if (line == "w") {
        playGame(GameState(b.gameBoard.shiftMatrixUp(), b.canPlay).gameBoard.play)
      } else if (line == "s") {
        playGame(GameState(b.gameBoard.shiftMatrixDown(), b.canPlay).gameBoard.play)
      } else {
        playGame(b)
      }
      fuckingBoard
    } else {
      println(b.gameBoard)
      println(
        """
          |##    ##  #######  ##     ##     ######  ##     ##  ######  ##    ##
          | ##  ##  ##     ## ##     ##    ##    ## ##     ## ##    ## ##   ##
          |  ####   ##     ## ##     ##    ##       ##     ## ##       ##  ##
          |   ##    ##     ## ##     ##     ######  ##     ## ##       #####
          |   ##    ##     ## ##     ##          ## ##     ## ##       ##  ##
          |   ##    ##     ## ##     ##    ##    ## ##     ## ##    ## ##   ##
          |   ##     #######   #######      ######   #######   ######  ##    ##
        """.stripMargin)
      GameState(b.gameBoard, false)
    }
  }

  case class Board(rows: List[List[Int]]) {
    override def toString: String = {
      val colorsMap = Map(
        0 -> Console.YELLOW,
        2 -> Console.BLUE,
        4 -> Console.RED,
        8 -> Console.MAGENTA,
        16 -> Console.CYAN,
        32 -> Console.WHITE,
        64 -> Console.GREEN
      ).withDefaultValue("")
      rows.map(row => row.map(aa => colorsMap(aa) + aa).mkString("\t")).mkString("\n")
    }

    //method to check if you can still play
    def play: GameState = {
      if (hasAnySpaceLeft) {
        GameState(Board(rows).insertFieldRandomly(), true)
      } else {
        GameState(this, false)
      }
    }

    //method which move all zeros at the beginning of the list
    def shiftZero(row: List[Int]): List[Int] = {
      val rowWithoutZero = row.filter(aa => aa != 0)
      val rowWithoutZeroSize = row.count(aa => aa != 0)
      if (rowWithoutZeroSize < row.size) {
        val listOfZeroSize = row.size - rowWithoutZeroSize
        List.fill(listOfZeroSize)(0) ++ rowWithoutZero
      } else {
        row
      }
    }
    //method to move row right; it contains all cases of adding the same numbers
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
      } else if (a2 == a3 && a2 != a1 && a0 != a1) {
        score += a2 + a3
        List(0, a0, a1, a2 + a3)
      } else if (a0 == a1 && a1 == a2 && a2 != a3) {
        score += a1 + a2
        List(0, a0, a1 + a2, a3)
      } else if (a1 == a2 && a2 == a3 && a0 != a1) {
        score += a2 + a3
        List(0, a0, a1, a2 + a3)
      } else if (a0 == a1 && a1 == a2 && a2 == a3) {
        score += a0 + a1 + a2 + a3
        List(0, 0, a0 + a1, a2 + a3)
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

    //adding random number (2 or 4) to matrix
    def insertFieldRandomly(): Board = {
      val random = scala.util.Random
      val randomNumber = (random.nextInt(2) + 1) * 2
      val rowsWithIndex = rows.map(aa => aa.zipWithIndex)
      val matrixWithIndex = rowsWithIndex.zipWithIndex.flatMap { case (rowWithIndex, rowIndex) =>
        rowWithIndex.filter({ case (value, idx) => value == 0 })
          .map { case (value, idx) => (rowIndex, idx) }
      }

      val randomPlace = random.nextInt(matrixWithIndex.size)
      val listWithIndex = matrixWithIndex(randomPlace)
      val randomColumn = listWithIndex._1
      val randomRow = listWithIndex._2

      Board(rows.updated(randomColumn, rows(randomColumn).updated(randomRow, randomNumber)))
    }

    //checking if you have place to move
    def hasAnySpaceLeft: Boolean = rows.flatten.contains(0)
  }

  object Board {
    def initializeBoard(): Board = {
      val rows = List(List.fill(4)(0), List.fill(4)(0), List.fill(4)(0), List.fill(4)(0))
      Board(rows).insertFieldRandomly().insertFieldRandomly()
    }
  }
  //class with flag if you can still play
  case class GameState(gameBoard: Board, canPlay: Boolean)

}
