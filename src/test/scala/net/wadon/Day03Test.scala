package net.wadon

import net.wadon.Day03.{Down, Left, Point, Right, Up}
import org.scalatest.{FreeSpec, Matchers}

class Day03Test extends FreeSpec with Matchers {
  "parseCommand" - {
    "should parse U command" in {
      Day03.parseCommand("U30") shouldBe Up(30)
    }
    "should parse D command" in {
      Day03.parseCommand("D11") shouldBe Down(11)
    }
    "should parse R command" in {
      Day03.parseCommand("R2") shouldBe Right(2)
    }
    "should parse L command" in {
      Day03.parseCommand("L76") shouldBe Left(76)
    }
  }

  "buildPoints" - {
    "should build points from previous point using Up command" in {
      val point = Point(0, 0)
      Day03.buildPoints(point, Up(2)) shouldBe List(Point(0, 1), Point(0, 2))
    }

    "should build points from previous point using Down command" in {
      val point = Point(2, 4)
      Day03.buildPoints(point, Down(4)) shouldBe List(
        Point(2, 3),
        Point(2, 2),
        Point(2, 1),
        Point(2, 0)
      )
    }

    "should build points from previous point using Left command" in {
      val point = Point(1, 2)
      Day03.buildPoints(point, Left(4)) shouldBe List(
        Point(0, 2),
        Point(-1, 2),
        Point(-2, 2),
        Point(-3, 2)
      )
    }

    "should build points from previous point using Right command" in {
      val point = Point(-1, -2)
      Day03.buildPoints(point, Right(3)) shouldBe List(
        Point(0, -2),
        Point(1, -2),
        Point(2, -2),
      )
    }
  }

  "buildPath" - {
    "should build path" in {
      val commands = List(Up(2), Right(3), Down(1))
      val points =
        List((0, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2), (3, 1)).map {
          case (x, y) => Point(x, y)
        }

      Day03.buildPath(commands) shouldBe points
    }
  }

  "minIntersectionDistance" - {
    "should calculate min distance 1" in {
      val commands1 = Day03.parseCommands("R75,D30,R83,U83,L12,D49,R71,U7,L72")
      val commands2 = Day03.parseCommands("U62,R66,U55,R34,D71,R55,D58,R83")

      val path1 = Day03.buildPath(commands1)
      val path2 = Day03.buildPath(commands2)

      Day03.minIntersectionDistance(path1, path2) shouldBe 159
    }

    "should calculate min distance 2" in {
      val commands1 =
        Day03.parseCommands("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      val commands2 =
        Day03.parseCommands("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

      val path1 = Day03.buildPath(commands1)
      val path2 = Day03.buildPath(commands2)

      Day03.minIntersectionDistance(path1, path2) shouldBe 135
    }
  }
}
