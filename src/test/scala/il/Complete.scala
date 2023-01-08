package il

import flux.Printer
import giraffe.{GLexer, GParser}

import scala.io.Source


class Complete extends munit.FunSuite {
  def check(filename: String)(implicit loc: munit.Location): Unit = {
    test(s"transform $filename") {
      val file = Source.fromInputStream(getClass.getResourceAsStream(filename)).mkString
      val sections = file.split("---").filter(_.nonEmpty).map(section => section(0) -> section.substring(1)).toMap

      val fStr = for {
        gTokens <- GLexer.lex(sections('g'))
        gAst <- GParser.parse(gTokens)
        fAst <- Transformer.transformProgram(gAst)
        fStr <- Right(Printer.print(fAst))
      } yield fStr

      val expected = sections.getOrElse('u', sections('f')).strip()

      // ---
      val obtained = fStr.map(_.mkString("\n")).getOrElse("").strip()
      pprint.pprintln(obtained)
      assertEquals(obtained, expected)
    }
  }

  check("/il/simplest")

  check("/il/common-queries/iot-common-queries/calculate-time-in-state")
}
