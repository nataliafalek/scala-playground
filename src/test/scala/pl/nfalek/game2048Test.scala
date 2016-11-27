package pl.nfalek

import org.scalatest.{FlatSpec, Matchers}
import pl.nfalek.game2048.{Board, GameOverException}

class game2048Test extends FlatSpec with Matchers {

  it should "shift right 1" in {
    val beforeShift = Board(List(
      List(0,0,2,0),
      List(2,2,2,2),
      List(0,0,2,4),
      List(0,0,0,0)
    ))

    val expected = Board(List(
      List(0,0,0,2),
      List(0,0,4,4),
      List(0,0,2,4),
      List(0,0,0,0)
    ))


    val shifted = beforeShift.shiftMatrixRight()

    shifted.rows(0) shouldBe expected.rows(0)
    shifted.rows(1) shouldBe expected.rows(1)
    shifted.rows(2) shouldBe expected.rows(2)
    shifted.rows(3) shouldBe expected.rows(3)
  }

  it should "shift left 1" in {
    val beforeShift = Board(List(
      List(0,0,2,0),
      List(2,2,2,2),
      List(0,0,2,4),
      List(0,0,0,0)
    ))

    val expected = Board(List(
      List(2,0,0,0),
      List(4,4,0,0),
      List(2,4,0,0),
      List(0,0,0,0)
    ))


    val shifted = beforeShift.shiftMatrixLeft()

    shifted.rows(0) shouldBe expected.rows(0)
    shifted.rows(1) shouldBe expected.rows(1)
    shifted.rows(2) shouldBe expected.rows(2)
    shifted.rows(3) shouldBe expected.rows(3)
  }

  it should "shift up 1" in {
    val beforeShift = Board(List(
      List(0,0,2,0),
      List(2,2,2,2),
      List(0,0,2,4),
      List(0,0,0,0)
    ))

    val expected = Board(List(
      List(2,2,4,2),
      List(0,0,2,4),
      List(0,0,0,0),
      List(0,0,0,0)
    ))


    val shifted = beforeShift.shiftMatrixUp()

    shifted.rows(0) shouldBe expected.rows(0)
    shifted.rows(1) shouldBe expected.rows(1)
    shifted.rows(2) shouldBe expected.rows(2)
    shifted.rows(3) shouldBe expected.rows(3)
  }

  it should "shift down 1" in {
    val beforeShift = Board(List(
      List(0,0,2,0),
      List(2,2,2,2),
      List(0,0,2,4),
      List(0,0,0,0)
    ))

    val expected = Board(List(
      List(0,0,0,0),
      List(0,0,0,0),
      List(0,0,2,2),
      List(2,2,4,4)
    ))


    val shifted = beforeShift.shiftMatrixDown()

    shifted.rows(0) shouldBe expected.rows(0)
    shifted.rows(1) shouldBe expected.rows(1)
    shifted.rows(2) shouldBe expected.rows(2)
    shifted.rows(3) shouldBe expected.rows(3)
  }

  it should "has any space 1 " in {
    val matrix = Board(List(
      List(2,4,2,8),
      List(8,16,64,32),
      List(4,4,2,0),
      List(0,4,16,2)
    ))

    matrix.hasAnySpaceLeft() shouldBe true
  }

  it should "hasn't space " in {
    val matrix = Board(List(
      List(2,2,2,8),
      List(2,2,2,2),
      List(2,16,2,4),
      List(2,4,2,4)
    ))

    matrix.hasAnySpaceLeft() shouldBe false
  }

  it should "exception test" in {
    val board = Board(List(
      List(2,4,2,8),
      List(8,16,64,32),
      List(4,16,2,4),
      List(2,4,16,2)
    ))
    // board do ktorego nei da sie nic wrzucic
    // board.moveLeft

    intercept[GameOverException] {
      board.play
    }
  }

  it should "play test" in {
    val board = Board(List(
      List(2,4,2,8),
      List(8,16,64,32),
      List(4,4,2,0),
      List(0,4,16,2)
    ))

    val expected = Board(List(
      List(2,4,2,8),
      List(8,16,64,32),
      List(8,2,0,0),
      List(4,16,12,0)
    ))

    board.play
  }
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

  //  val matrix = Board(List(
  //    List(0,0,2,0),
  //    List(2,2,2,2),
  //    List(0,0,2,4),
  //    List(0,0,0,0)
  //  ))

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

}
