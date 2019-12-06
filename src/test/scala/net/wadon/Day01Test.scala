package net.wadon

import org.scalatest._

class Day01Test extends FreeSpec with Matchers {
  "requiredFuel" - {
    "for a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2" in {
      Day01.requiredFuel(12) shouldBe 2
    }

    "for a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2" in {
      Day01.requiredFuel(14) shouldBe 2
    }

    "for a mass of 1969, the fuel required is 654" in {
      Day01.requiredFuel(1969) shouldBe 654
    }

    "for a mass of 100756, the fuel required is 33583" in {
      Day01.requiredFuel(100756) shouldBe 33583
    }
  }

  "requiredFuelRecursive" - {
    "for a mass of 14 the total fuel required is still just 2" in {
      Day01.requiredFuelRecursive(14) shouldBe 2
    }

    "for a mass of 1969 the total fuel required is 966" in {
      Day01.requiredFuelRecursive(1969) shouldBe 966
    }

    "for a mass of 100756 the total fuel required is 50346" in {
      Day01.requiredFuelRecursive(100756) shouldBe 50346
    }
  }
}
