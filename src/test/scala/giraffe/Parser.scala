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
  test("map with identifier") {
    val expr = WorkFlow.run("from x |. q w")
    assertEquals(expr, null)
  }
  test("map without identifier") {
    val expr = WorkFlow.run("from x |. w")
    assertEquals(expr, null)
  }
  test("common-queries/operate-on-columns/#calculate-a-new-column"){
    val expr = WorkFlow.run("from noaa |$ \"average_temperature\" |@ start \"-30d\" |. q() ")
    assertEquals(expr, null)
  }
  test("map call with no identifier"){
    val expr = WorkFlow.run("from a |. q ( v )")
    assertEquals(expr, null)
  }

  test("map call with identifier") {
    val expr = WorkFlow.run("from a |. b q ( v )")
    assertEquals(expr, null)
  }

  test("map call wtih multiple args") {
    val expr = WorkFlow.run("from a |. q ( a0 , a1 )")
    assertEquals(expr, null)
  }

  test("literals")  {
    val expr = WorkFlow.run("from a | 12")
    assertEquals(expr, null)
  }
}
