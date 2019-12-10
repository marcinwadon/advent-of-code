package net.wadon

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.Stream

object Day03 extends IOApp {

  sealed trait Command
  case class Up(n: Int) extends Command
  case class Down(n: Int) extends Command
  case class Right(n: Int) extends Command
  case class Left(n: Int) extends Command

  case class Point(x: Int, y: Int)

  val centralPoint = Point(0, 0)

  def parseCommand(command: String): Command = command.splitAt(1) match {
    case (c, n) =>
      val moves = n.toInt
      c match {
        case "U" => Up(moves)
        case "D" => Down(moves)
        case "R" => Right(moves)
        case "L" => Left(moves)
      }
  }

  def parseCommands(commands: String): List[Command] =
    commands.split(",").toList.map(parseCommand)

  def buildPoints(point: Point, command: Command): List[Point] = {
    val tuple = command match {
      case Up(n)    => (0, 1, n)
      case Down(n)  => (0, -1, n)
      case Right(n) => (1, 0, n)
      case Left(n)  => (-1, 0, n)
    }

    tuple match {
      case (dx, dy, n) =>
        List.range(1, n + 1).map { i =>
          Point(point.x + dx * i, point.y + dy * i)
        }
    }
  }

  def buildPath(commands: List[Command]): List[Point] = {
    commands.foldLeft(List.empty[Point])(
      (l, c) => l ++ buildPoints(l.lastOption.getOrElse(centralPoint), c)
    )
  }

  def manhattanDistance(p1: Point)(p2: Point): Int =
    (p1.x - p2.x).abs + (p1.y - p2.y).abs

  def minIntersectionDistance(path1: List[Point], path2: List[Point]): Int =
    path1.intersect(path2).map(manhattanDistance(centralPoint)).min

  val path = getClass.getResource("/Day03.txt").getPath

  val reader: Stream[IO, String] = FileReader
    .reader(path)

  val part1 = reader
    .map(parseCommands)
    .map(buildPath)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      p1 <- part1.compile.toList
      a = p1.head
      b = p1.last
      result1 = minIntersectionDistance(a, b)
      _ <- IO { println(result1) }
    } yield ExitCode.Success
}
