import flux.{Printer, Renderer}
import giraffe.{GLexer, GParser}
import il.Transformer
import zio.*
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli.*

import java.nio.file.Path

object girafflux extends ZIOCliDefault {

  val inArg = Args.file("in", Exists.Yes)
  val outArg = Args.file("out", Exists.Either)

  val gflx: Command[(Path, Path)] = Command("gflx", args = inArg ++ outArg)

  val execute: (Path, Path) => UIO[Unit] = {
    (inPath, outPath) => {
      for {
        inContent <- ZIO.readFile(inPath)
        _ <- printLine(inContent)
        gTokens <- ZIO.fromEither(GLexer.lex(inContent))
        gAst <- ZIO.fromEither(GParser.parse(gTokens))
        fAst <- ZIO.fromEither(Transformer.transformProgram(gAst))
        fRender <- ZIO.succeed(Renderer.render(fAst))
        rendered <- ZIO.succeed(Printer().print(fRender).s)
        _ <- ZIO.writeFile(outPath, rendered)
      } yield()
    }.orDie
  }

  val cliApp = CliApp.make(
    name = "Girafflux",
    version = "0.0.1",
    summary = text("A language which helps you be more productive with Flux"),
    command = gflx
  )(execute.tupled)
}
