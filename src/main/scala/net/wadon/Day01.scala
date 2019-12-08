package net.wadon

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream

object Day01 extends IOApp {
  import YCombinator._

  def requiredFuel(n: Int): Int = Math.max(0, (n / 3).floor.toInt - 2)

  def requiredFuelFactory(fn: Int => Int): Int => Int = { n =>
    if (n <= 0) 0
    else {
      val fuel = requiredFuel(n)
      fuel + fn(fuel)
    }
  }

  def requiredFuelRecursive(n: Int): Int = Y(requiredFuelFactory)(n)

  val path = getClass.getResource("/Day01.txt").getPath

  val reader: Stream[IO, String] = FileReader.reader(path)

  val part1: Stream[IO, Int] =
    reader
      .map(line => requiredFuel(line.toInt))
      .scan(0)(_ + _)

  val part2: Stream[IO, Int] =
    reader
      .map(line => requiredFuelRecursive(line.toInt))
      .scan(0)(_ + _)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      p1 <- part1.compile.toList.map(_.last)
      p2 <- part2.compile.toList.map(_.last)
      _ <- IO { println((p1, p2)) }
    } yield ExitCode.Success
}
