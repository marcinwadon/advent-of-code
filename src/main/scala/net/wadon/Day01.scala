package net.wadon

import java.nio.file.Paths

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{Stream, io, text}

object YCombinator {
  def Y[T](func: (T => T) => (T => T)): (T => T) = func(Y(func))(_: T)
}

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

  val reader: Stream[IO, String] = Stream.resource(Blocker[IO]).flatMap {
    blocker =>
      io.file
        .readAll[IO](Paths.get(path), blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
        .filter(s => !s.trim.isEmpty)
  }

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
      _ <- IO { println(p1, p2) }
    } yield ExitCode.Success
}
