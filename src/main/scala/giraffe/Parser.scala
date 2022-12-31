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

  def program: Parser[gExpr] = {
    phrase(script)
  }

  def script: Parser[gExpr.Script] = positioned {
    (rep(moduleImport) ~ rep(query)) ^^ {case is ~ qs => gExpr.Script(is, qs) }
  }

  def moduleImport: Parser[gExpr.ModuleImport] = positioned {
    (gToken.ModuleImport() ~> litStr) ^^ { case s => gExpr.ModuleImport(s) }
  }

  def query: Parser[gExpr.Query] = positioned {
    (gToken.From() ~ identifier ~ stages) ^^ { case _ ~ bucket ~ stages => gExpr.Query(gExpr.From(bucket), stages) }
  }

  def block: Parser[gExpr.Block] = positioned {
    (call | index | identifier | lit) ^^ { case i => gExpr.Block.lift(i)}
  }

  def call: Parser[gExpr.Call] = positioned {
    (identifier ~ gToken.ParenL() ~ repsep(arg, gToken.Comma()) ~ gToken.ParenR()) ^^ { case i ~ _ ~ args ~ _ => gExpr.Call(i, args) }
  }

  def arg: Parser[gExpr.Arg] =  {
    (identifier ~ gToken.Colon() ~ (block | implicitRef) ) ^^ { case i ~ _ ~ v => gExpr.Arg(i, v)}
  }

  def index: Parser[gExpr.Index] = {
    ((identifier | implicitRef) ~ gToken.Period() ~ block) ^^ { case obj ~ _ ~ value => gExpr.Index(obj, value)}
  }

  def stages: Parser[List[gExpr.gStage]] = {
    rep1(stage)
  }

  def stage: Parser[gExpr.gStage] = positioned {
    (gToken.Pipe() ~ (stageStreamMap | stageFilterMeasurement | stageFilterFieldMany | stageFilterField | stageRange | stageMap )) ^^ { case _ ~ s => s }
  }

  def stageStreamMap: Parser[gExpr.gStage.streamMap] = positioned {
    call ^^ (c => gExpr.gStage.streamMap(c))
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

  def stageFilterFieldMany: Parser[gExpr.gStage.filterFieldMany] = positioned {
    (gToken.Percent() ~> repsep(lit, gToken.Comma())) ^^ (is => gExpr.gStage.filterFieldMany(_fields = is))
  }

  def stageMap: Parser[gExpr.gStage.map] = positioned {
    val withIdentifier = (gToken.Period() ~> identifier ~ block) ^^ { case i ~ b => gExpr.gStage.map(Some(i), b) }
    val withoutIdentifier = (gToken.Period() ~> block) ^^ (b => gExpr.gStage.map(None, b))
    withIdentifier | withoutIdentifier
  }

  private def identifier: Parser[gExpr.Id] = positioned {
    accept("identifier", { case id : gToken.Id => gExpr.Id(id) })
  }

  private def litStr = {
    accept("string literal", { case s: gToken.LitStr => gExpr.gLit.Str(s) })
  }

  def litArray: Parser[gExpr.gLit.Array] = positioned {
    gToken.BracketL() ~> repsep(block, gToken.Comma()) <~ gToken.BracketR() ^^ (items => gExpr.gLit.Array(items))
  }

  def lit: Parser[gExpr.gLit] = positioned {
    litStr
    | accept("float literal", { case f: gToken.LitFloat => gExpr.gLit.Float(f) })
    | accept("int literal", { case i: gToken.LitInt => gExpr.gLit.Int(i) })
    | litArray
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
