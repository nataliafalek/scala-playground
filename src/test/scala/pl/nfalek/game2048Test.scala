package pl.nfalek

import org.scalatest.{FlatSpec, Matchers}
import pl.nfalek.game2048.Board

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

}
