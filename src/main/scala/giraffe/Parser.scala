package giraffe

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object GParser extends Parsers {
  override type Elem = gToken

  def parse(tokens: Seq[gToken]): Either[gParseError, gExpr.Script] = {
    val reader = new gTokenReader(tokens)
    program(reader) match
      case NoSuccess(msg, next) => Left(gParseError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
  }

  // parsers

  def program: Parser[gExpr.Script] = {
    phrase(script)
  }

  def script: Parser[gExpr.Script] = positioned {
    (rep(moduleImport) ~ rep(query)) ^^ {case is ~ qs => gExpr.Script(is, qs) }
  }

  def moduleImport: Parser[gExpr.ModuleImport] = positioned {
    (gToken.ModuleImport() ~> litStr) ^^ (s => gExpr.ModuleImport(s))
  }

  def query: Parser[gExpr.Query] = positioned {
    (gToken.From() ~ identifier ~ stages) ^^ { case _ ~ bucket ~ stages => gExpr.Query(gExpr.From(bucket), stages) }
  }

  private def blockMany = {
    gToken.BraceL() ~> rep(blocklike) <~ gToken.BraceR() ^^ (exprs => gExpr.Block(exprs))
  }

  def blocklike: Parser[gExpr.blocklike] = positioned {
    ((call | assign | index | identifier | lit) ^^ { case i: gExpr.blocklike => i})
      | blockMany
  }

  def call: Parser[gExpr.Call] = positioned {
    (index ~ gToken.ParenL() ~ repsep(arg, gToken.Comma()) ~ gToken.ParenR()) ^^ { case i ~ _ ~ args ~ _ => gExpr.Call(i, args) }
    | (identifier ~ gToken.ParenL() ~ repsep(arg, gToken.Comma()) ~ gToken.ParenR()) ^^ { case i ~ _ ~ args ~ _ => gExpr.Call(i, args) }
  }

  def arg: Parser[gExpr.Arg] =  {
    (identifier ~ gToken.Colon() ~ blocklike) ^^ { case i ~ _ ~ v => gExpr.Arg(i, v)}
    | (identifier ~ gToken.Colon() ~ implicitRef) ^^ { case i ~ _ ~ v => gExpr.Arg(i, v)}
  }

  def index: Parser[gExpr.Index] = {
    val chainFirst = (implicitRef) ~ gToken.Period() ~ identifier ^^ { case l ~ _ ~ r => gExpr.Index(l, r) }
       | (identifier) ~ gToken.Period() ~ identifier ^^ { case l ~ _ ~ r => gExpr.Index(l, r) }

    chainl1(
      chainFirst,
      identifier,
      gToken.Period() ^^^ { (l: gExpr.assignable, r: gExpr.Id) => gExpr.Index(l, r) }
    )
  }

  def stages: Parser[List[gExpr.gStage]] = {
    rep1(stage)
  }

  def stage: Parser[gExpr.gStage] = positioned {
    (gToken.Pipe() ~ (stageStreamMap | stageFilterMeasurement | stageFilterFieldMany | stageFilterField | stageRange | stageMap | stageMapMany )) ^^ { case _ ~ s => s }
  }

  def stageStreamMap: Parser[gExpr.gStage.streamMap] = positioned {
    call ^^ (c => gExpr.gStage.streamMap(gExpr.Block.lift(c)))
  }

  def stageRange: Parser[gExpr.gStage.range] = positioned {
    (gToken.Atpersat() ~> gToken.Id("start") ~> lit ~ opt(gToken.Id("stop") ~> lit)) ^^ {
      case start ~ Some(stop) => gExpr.gStage.range(gExpr.Block.lift(start), gExpr.Block.lift(stop))
      case start ~ None => gExpr.gStage.range(gExpr.Block.lift(start))
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
    val withIdentifier = (gToken.Period() ~> identifier ~ blocklike) ^^ { case i ~ b => gExpr.gStage.map(i, b) }
    val withoutIdentifier = (gToken.Period() ~> implicitRef ~ blocklike) ^^ { case i ~ b => gExpr.gStage.map(i, b) }
    withIdentifier | withoutIdentifier
  }

  def stageMapMany: Parser[gExpr.gStage.mapMany] = positioned {
    val replacing = (gToken.Period() ~> implicitRef ~ litRecord) ^^ { case i ~ b => gExpr.gStage.mapMany(Some(i), b)}
    val adding = (gToken.Period() ~> litRecord) ^^ (b => gExpr.gStage.mapMany(None, b))
    val replacingBlock = (gToken.Period() ~> implicitRef ~ blockMany) ^^ { case i ~ b => gExpr.gStage.mapMany(Some(i), b)}
    val addingBlock = (gToken.Period() ~> blockMany) ^^ (b => gExpr.gStage.mapMany(None, b))
    replacingBlock | replacing | addingBlock | adding
  }

  private def identifier: Parser[gExpr.Id] = positioned {
    accept("identifier", { case id : gToken.Id => gExpr.Id(id) })
  }

  private def litStr = {
    accept("string literal", { case s: gToken.LitStr => gExpr.gLit.Str(s) })
  }

  def litArray: Parser[gExpr.gLit.Array] = positioned {
    gToken.BracketL() ~> repsep(blocklike, gToken.Comma()) <~ gToken.BracketR() ^^ (items => gExpr.gLit.Array(items))
  }

  def litRecord: Parser[gExpr.gLit.Record] = positioned {
    (gToken.BraceL() ~> repsep(identifier ~ gToken.Colon() ~ blocklike, gToken.Comma()) <~ gToken.BraceR()) ^^ (kvpairs => gExpr.gLit.Record(kvpairs.map { case k ~ _ ~ v => k -> v }.toMap))
  }

  def lit: Parser[gExpr.gLit] = positioned {
    litStr
    | accept("float literal", { case f: gToken.LitFloat => gExpr.gLit.Float(f) })
    | accept("int literal", { case i: gToken.LitInt => gExpr.gLit.Int(i) })
    | accept("dateTime literal", {case td: gToken.LitDuration => gExpr.gLit.Duration(td)})
    | litArray
    | litRecord
  }

  def implicitRef: Parser[gExpr.ImplicitRef] = positioned {
    gToken.Underscore() ^^^ gExpr.ImplicitRef()
  }

  def assign: Parser[gExpr.Assign] = positioned {
    (index ~ gToken.Equal() ~ blocklike) ^^ { case obj ~ _ ~ value => gExpr.Assign(obj, value)}
    | (implicitRef ~ gToken.Equal() ~ blocklike) ^^ { case obj ~ _ ~ value => gExpr.Assign(obj, value)}
    | (identifier ~ gToken.Equal() ~ blocklike) ^^ { case obj ~ _ ~ value => gExpr.Assign(obj, value)}
  }

}

class gTokenReader(tokens: Seq[gToken]) extends Reader[gToken] {
  override def first: gToken = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[gToken] = new gTokenReader(tokens.tail)
}
