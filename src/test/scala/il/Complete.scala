package il

import flux.{Renderer, Printer}
import giraffe.{GLexer, GParser}

import scala.io.Source


class Complete extends munit.FunSuite {
  def check(filename: String)(implicit loc: munit.Location): (String, String) = {
    val file = Source.fromInputStream(getClass.getResourceAsStream(filename)).mkString
    val sections = file.split("---").filter(_.nonEmpty).map(section => section(0) -> section.substring(1)).toMap

    val rendered = for {
      gTokens <- GLexer.lex(sections('g'))
      gAst <- {
        val o = GParser.parse(gTokens)
        pprint.pprintln(o)
        o
      }
      fAst <- Transformer.transformProgram(gAst)
      fStr <- Right(Renderer.render(fAst))
      rendered <- Right(Printer().print(fStr).s)
    } yield rendered

    val expected = sections.getOrElse('u', sections('f')).strip()

    // ---
    val obtained = rendered.getOrElse("").strip()
    pprint.pprintln(obtained)
    (obtained, expected)
  }

  test("0") {
    val (obtained, expected) = check ("/il/common-queries/operate-on-columns/find-unique-values.test")
    assertEquals(obtained, expected)
  }
  test("1") {
    val (obtained, expected) = check("/il/common-queries/operate-on-columns/count-unique-values.test")
    assertEquals(obtained, expected)
  }
  test("2") {
    val (obtained, expected) = check("/il/common-queries/operate-on-columns/recalculate-the-_values-column.test")
    assertEquals(obtained, expected)
  }
  test("3") {
    val (obtained, expected) = check("/il/common-queries/iot-common-queries/calculate-time-in-state.test")
    assertEquals(obtained, expected)
  }
  test("4") {
    val (obtained, expected) = check("/il/universe/filter/filter-based-on-influxdb-measurement-field-and-tag.test")
    assertEquals(obtained, expected)
  }
  test ("5") {
    val (obtained, expected) = check("/il/t.test")
    assertEquals(obtained, expected)
  }
}
