package pl.nfalek

import org.scalatest.{FlatSpec, Matchers}

class NatiSampleTest extends FlatSpec with Matchers {

  it should "add two numbers" in {
    val addResult = NatiSample.add(2, 4)
    addResult shouldBe 6
  }
}
