package giraffe

class Lexer extends munit.FunSuite {
  test("anything") {
    val expr = GLexer.lex("()")
    assert(expr.isRight)
  }
}
