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
    (gToken.From() ~ identifier ~ stage) ^^ { case _ ~ bucket ~ stage => gExpr.Query(gExpr.From(gExpr.Id(bucket)), List(stage)) }
  }

  def stage: Parser[gExpr.gStage] = positioned {
    (gToken.Pipe()) ^^ { case pipe => gExpr.gStage.aaa() }
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
