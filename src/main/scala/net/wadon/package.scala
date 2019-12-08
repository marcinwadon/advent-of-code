package net

import java.nio.file.Paths

import cats.effect.{Blocker, ContextShift, IO}
import fs2.{Stream, io, text}

package object wadon {
  object YCombinator {
    def Y[T](func: (T => T) => (T => T)): (T => T) = func(Y(func))(_: T)
  }

  object FileReader {
    def reader(path: String)(implicit C: ContextShift[IO]): Stream[IO, String] =
      Stream.resource(Blocker[IO]).flatMap { blocker =>
        io.file
          .readAll[IO](Paths.get(path), blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .filter(s => !s.trim.isEmpty)
      }
  }
}
