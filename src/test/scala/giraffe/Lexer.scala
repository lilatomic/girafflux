package giraffe

class Lexer extends munit.FunSuite {
  test("anything") {
    val expr = GLexer.lex("()")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("Datetime - absolute") {
    val expr = GLexer.lex("2011-11-11T11:11:11.111Z")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("Datetime - offset") {
    val expr = GLexer.lex("2011-11-11T11:11:11.111+11:11")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
}
