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

  def block: Parser[gExpr.Block] = positioned {
    val _call = call ^^ { case i => gExpr.Block.lift(i) }
    val _id = identifier ^^ { case i => gExpr.Block.lift(gExpr.Id(i)) }
    val _literal = lit ^^ { case i => gExpr.Block.lift(i)}
    (_call | _id | _literal)
  }

  def call: Parser[gExpr.Call] = positioned {
    (identifier ~ gToken.ParenL() ~ repsep(block | implicitRef, gToken.Comma()) ~ gToken.ParenR()) ^^ { case i ~ _ ~ args ~ _ => gExpr.Call(gExpr.Id(i), args) }
  }

  def stages: Parser[List[gExpr.gStage]] = {
    rep1(stage)
  }

  def stage: Parser[gExpr.gStage] = positioned {
    (gToken.Pipe() ~ (stageStreamMap | stageFilterMeasurement | stageFilterField | stageRange | stageMap )) ^^ { case _ ~ s => s }
  }

  def stageStreamMap: Parser[gExpr.gStage.streamMap] = positioned {
    (lit) ^^ (lit => gExpr.gStage.streamMap(lit))
  }

  def stageRange: Parser[gExpr.gStage.range] = positioned {
    (gToken.Atpersat() ~> gToken.Id("start") ~> lit ~ opt(gToken.Id("stop") ~> lit)) ^^ {
      case start ~ Some(stop) => gExpr.gStage.range(start, stop)
      case start ~ None => gExpr.gStage.range(start)
    }
  }

  def stageFilterMeasurement: Parser[gExpr.gStage.filterMeasurement] = positioned {
    (gToken.Dollar() ~> lit) ^^ (i => gExpr.gStage.filterMeasurement(_measurement = i))
  }

  def stageFilterField: Parser[gExpr.gStage.filterField] = positioned {
    (gToken.Percent() ~> lit) ^^ (i => gExpr.gStage.filterField(_field = i))
  }

  def stageMap: Parser[gExpr.gStage.map] = positioned {
    val withIdentifier = (gToken.Period() ~> identifier ~ block) ^^ { case i ~ b => gExpr.gStage.map(Some(gExpr.Id(i)), b) }
    val withoutIdentifier = (gToken.Period() ~> block) ^^ (b => gExpr.gStage.map(None, b))
    withIdentifier | withoutIdentifier
  }

  private def identifier: Parser[gToken.Id] = positioned {
    accept("identifier", { case id@gToken.Id(name) => id })
  }

  def lit: Parser[gExpr.gLit] = positioned {
    accept("string literal", { case s: gToken.LitStr => gExpr.gLit.Str(s) })
    | accept("float literal", { case f: gToken.LitFloat => gExpr.gLit.Float(f) })
    | accept("int literal", { case i: gToken.LitInt => gExpr.gLit.Int(i) })
  }

  def implicitRef: Parser[gExpr.ImplicitRef] = positioned {
    gToken.Underscore() ^^^ gExpr.ImplicitRef()
  }

}

class gTokenReader(tokens: Seq[gToken]) extends Reader[gToken] {
  override def first: gToken = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[gToken] = new gTokenReader(tokens.tail)
}
