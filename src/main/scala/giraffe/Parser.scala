package giraffe

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object GParser extends Parsers {
  override type Elem = gToken

  def parse(tokens: Seq[gToken]) = {
    val reader = new gTokenReader(tokens)
    program(reader) match
      case NoSuccess(msg, next) => Left(gParseError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
  }

  // parsers


  def program: Parser[gExpr] = positioned {
    phrase(query)
  }

  def query: Parser[gExpr.Query] = positioned {
    (gToken.From() ~ identifier ~ stages) ^^ { case _ ~ bucket ~ stages => gExpr.Query(gExpr.From(gExpr.Id(bucket)), stages) }
  }

  def stages: Parser[List[gExpr.gStage]] = {
    rep1(stage)
  }

  def stage: Parser[gExpr.gStage] = positioned {
    (gToken.Pipe() ~ (stageAaa | stageBbb)) ^^ { case _ ~ s => s}
  }

  def stageAaa: Parser[gExpr.gStage.aaa] = positioned {
    (lit) ^^ (lit => gExpr.gStage.aaa(lit.s))
  }

  def stageBbb: Parser[gExpr.gStage.bbb] = positioned {
    (gToken.Atpersat() ~ gToken.Id("start") ~ lit) ^^ { case _ ~ _ ~ lit => gExpr.gStage.bbb(lit.s)}
  }

  private def identifier: Parser[gToken.Id] = positioned {
    accept("identifier", { case id@gToken.Id(name) => id })
  }

  def lit: Parser[gToken.Lit] = positioned {
    accept("string literal", { case lit@gToken.Lit(s) => lit })
  }

}

class gTokenReader(tokens: Seq[gToken]) extends Reader[gToken] {
  override def first: gToken = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[gToken] = new gTokenReader(tokens.tail)
}
