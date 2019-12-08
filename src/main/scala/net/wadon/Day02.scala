package net.wadon

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream

import scala.annotation.tailrec

object Day02 extends IOApp {
  @tailrec
  def intcode(code: List[Int], pos: Int = 0): List[Int] =
    code.splitAt(pos)._2 match {
      case 99 :: _ => code
      case 1 :: x :: y :: z :: _ => {
        val sum = List(code.get(x), code.get(y)).sequence.get.sum
        intcode(code.patch(z, Seq(sum), 1), pos + 4)
      }
      case 2 :: x :: y :: z :: _ => {
        val mul = List(code.get(x), code.get(y)).sequence.get.product
        intcode(code.patch(z, Seq(mul), 1), pos + 4)
      }
      case _ => throw new IllegalArgumentException("")
    }

  val path = getClass.getResource("/Day02.txt").getPath

  val reader: Stream[IO, List[Int]] = FileReader
    .reader(path)
    .map(line => line.split(",").toList.map(_.toInt))

  val part1: Stream[IO, List[Int]] =
    reader
      .map(codes => codes.patch(1, Seq(12, 2), 2))
      .map(intcode(_, 0))

  val inputs: Stream[IO, (Int, Int)] =
    Stream
      .range[IO](0, 100)
      .flatMap(n => Stream.range[IO](0, 100).map((n, _)))

  val part2: Stream[IO, (Int, Int, Int)] =
    reader
      .flatMap { codes =>
        inputs.map {
          case (noun, verb) =>
            (noun, verb, intcode(codes.patch(1, Seq(noun, verb), 2), 0).head)
        }
      }
      .find {
        case (_, _, r) => r == 19690720
      }

  val part3 = part2.map {
    case (noun, verb, _) => 100 * noun + verb
  }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      p1 <- part1.compile.toList
      p2 <- part2.compile.toList
      p3 <- part3.compile.toList
      _ <- IO { println((p2, p3)) }
    } yield ExitCode.Success
}
