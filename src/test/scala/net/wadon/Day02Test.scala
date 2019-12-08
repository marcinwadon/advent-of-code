package net.wadon

import org.scalatest._

class Day02Test extends FreeSpec with Matchers {
  "intcode computer" - {
    "List(1,0,0,0,99) becomes List(2,0,0,0,99)" in {
      Day02.intcode(List(1, 0, 0, 0, 99)) shouldBe List(2, 0, 0, 0, 99)
    }
    "List(2,3,0,3,99) becomes List(2,3,0,6,99)" in {
      Day02.intcode(List(2, 3, 0, 3, 99)) shouldBe List(2, 3, 0, 6, 99)
    }
    "List(2,4,4,5,99) becomes List(2,4,4,5,99,9801)" in {
      Day02.intcode(List(2, 4, 4, 5, 99)) shouldBe List(2, 4, 4, 5, 99, 9801)
    }
    "List(1,1,1,4,99,5,6,0,99) becomes List(30,1,1,4,2,5,6,0,99)" in {
      Day02.intcode(List(1, 1, 1, 4, 99, 5, 6, 0, 99)) shouldBe List(30, 1, 1,
        4, 2, 5, 6, 0, 99)
    }
  }
}
