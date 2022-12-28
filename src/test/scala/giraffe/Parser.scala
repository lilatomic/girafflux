package giraffe

object WorkFlow {
  def run(code: String): Either[gCompilationError, gExpr] = {
    for {
      tokens <- GLexer.lex(code)
      ast <- GParser.parse(tokens)
    } yield ast
  }
}

class Parser extends munit.FunSuite {
  test("anything") {
    val expr = WorkFlow.run("from x | \"zzz\" |@ start \"y0\" stop \"y1\" |@ start \"x0\"")
    assertEquals(expr, null)
  }
  test("common-queries/operate-on-columns/#calculate-a-new-column"){
    val expr = WorkFlow.run("from noaa |$ \"average_temperature\" |@ start \"-30d\" | \"???\"")
    assertEquals(expr, null)
  }
}
