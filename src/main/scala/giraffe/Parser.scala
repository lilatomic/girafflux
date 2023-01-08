package giraffe

import giraffe.gExpr.Block

import scala.annotation.tailrec
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object GParser extends Parsers {
  override type Elem = gToken

  def parse(tokens: Seq[gToken]): Either[gParseError, gExpr.Script] = {
    val reader = new gTokenReader(tokens)

    def error(msg: String, next: Input) = Left(gParseError(Location(next.pos.line, next.pos.column), msg))

    program(reader) match
      case NoSuccess(msg, next) => error(msg, next)
      case Success(result, _) => Right(result)
      case Failure(msg, next) => error(msg, next)
      case Error(msg, next) => error(msg, next)
  }

  // parsers

  private def program: Parser[gExpr.Script] = {
    phrase(script)
  }

  private def script: Parser[gExpr.Script] = positioned {
    (rep(moduleImport) ~ rep(query)) ^^ { case is ~ qs => gExpr.Script(is, qs) }
  }

  def moduleImport: Parser[gExpr.ModuleImport] = positioned {
    (gToken.ModuleImport() ~> litStr) ^^ (s => gExpr.ModuleImport(s))
  }

  private def query: Parser[gExpr.Query] = positioned {
    (gToken.From() ~ identifier ~ stages) ^^ { case _ ~ bucket ~ stages => gExpr.Query(gExpr.From(bucket), stages) }
  }

  private def blockMany = {
    gToken.BraceL() ~> rep(blocklike) <~ gToken.BraceR() ^^ (exprs => gExpr.Block(exprs))
  }

  def blocklike: Parser[gExpr.blocklike] = positioned {
    call | assign | member | identifier | lit | blockMany
  }

  private def call: Parser[gExpr.Call] = positioned {
    (member ~ gToken.ParenL() ~ repsep(arg, gToken.Comma()) ~ gToken.ParenR()) ^^ { case i ~ _ ~ args ~ _ => gExpr.Call(i, args) }
      | (identifier ~ gToken.ParenL() ~ repsep(arg, gToken.Comma()) ~ gToken.ParenR()) ^^ { case i ~ _ ~ args ~ _ => gExpr.Call(i, args) }
  }

  private def arg: Parser[gExpr.Arg] = {
    (identifier ~ gToken.Colon() ~ blocklike) ^^ { case i ~ _ ~ v => gExpr.Arg(i, v) }
      | (identifier ~ gToken.Colon() ~ implicitRef) ^^ { case i ~ _ ~ v => gExpr.Arg(i, v) }
  }

  private def member: Parser[gExpr.Member] = {
    val chainFirst = implicitRef ~ gToken.Period() ~ identifier ^^ { case l ~ _ ~ r => gExpr.Member(l, r) }
      | identifier ~ gToken.Period() ~ identifier ^^ { case l ~ _ ~ r => gExpr.Member(l, r) }

    chainl1(
      chainFirst,
      identifier,
      gToken.Period() ^^^ { (l: gExpr.assignable, r: gExpr.Id) => gExpr.Member(l, r) }
    )
  }

  private def stages: Parser[List[gExpr.gStage]] = {
    rep1(stage)
  }

  private def stage: Parser[gExpr.gStage] = positioned {
    (gToken.Pipe() ~ (stageStreamMap | stageFilterMeasurement | stageFilterField | stageRange | stageMap | stageMapMany)) ^^ { case _ ~ s => s }
  }

  private def stageStreamMap: Parser[gExpr.gStage.streamMap] = positioned {
    call ^^ (c => gExpr.gStage.streamMap(gExpr.Block.lift(c)))
  }

  private def stageRange: Parser[gExpr.gStage.range] = positioned {
    (gToken.Atpersat() ~> gToken.Id("start") ~> lit ~ opt(gToken.Id("stop") ~> lit)) ^^ {
      case start ~ Some(stop) => gExpr.gStage.range(gExpr.Block.lift(start), gExpr.Block.lift(stop))
      case start ~ None => gExpr.gStage.range(gExpr.Block.lift(start))
    }
  }

  private def stageFilterMeasurement: Parser[gExpr.gStage.filterMeasurement] = positioned {
    (gToken.Dollar() ~> lit) ^^ (i => gExpr.gStage.filterMeasurement(_measurement = i))
  }

  private def stageFilterField: Parser[gExpr.gStage.filterFieldMany | gExpr.gStage.filterField] = positioned {
    (gToken.Percent() ~> repsep(lit, gToken.Comma())) ^^ (is =>
      is match
        case i :: Nil => gExpr.gStage.filterField(i)
        case _ => gExpr.gStage.filterFieldMany(is)
      )
  }

  private def stageMap: Parser[gExpr.gStage.map] = positioned {
    val withIdentifier = (gToken.Period() ~> identifier ~ blocklike) ^^ { case i ~ b => gExpr.gStage.map(i, b) }
    val withoutIdentifier = (gToken.Period() ~> implicitRef ~ blocklike) ^^ { case i ~ b => gExpr.gStage.map(i, b) }
    withIdentifier | withoutIdentifier
  }

  private def stageMapMany: Parser[gExpr.gStage.map | gExpr.gStage.mapMany] = positioned {
    val replacingMany = (gToken.Period() ~> implicitRef ~ litRecord) ^^ { case i ~ b => gExpr.gStage.mapMany(Some(i), None, b) }
    val addingMany = (gToken.Period() ~> litRecord) ^^ (b => gExpr.gStage.mapMany(None, None, b))

    def walkBlockForReturnResult(block: gExpr.Block): Option[(gExpr.Block, gExpr.gLit.Record)] =
      block.exprs match
        case ::(_, _) =>
          block.exprs.last match
            case b: gExpr.Block => walkBlockForReturnResult(b).map(
              (inner: gExpr.Block, r: gExpr.gLit.Record) => (gExpr.Block(block.exprs.init :+ inner), r)
            )
            case r: gExpr.gLit.Record => Some(Block(block.exprs.init), r)
            case _ => None
        case Nil => None

    val inBlock = (gToken.Period() ~> opt(implicitRef) ~ blockMany) >> {
      case i ~ b =>
        val lastInBlockIsARecord = walkBlockForReturnResult(b)
        lastInBlockIsARecord match
          case Some(newBlock, r) => success(gExpr.gStage.mapMany(i, Some(newBlock), r))
          case None => err("mapMany must have a record as the last (nested) expression in the block")
    }

    replacingMany | addingMany | inBlock
  }

  private def identifier: Parser[gExpr.Id] = positioned {
    accept("identifier", { case id: gToken.Id => gExpr.Id(id) })
  }

  private def litStr = {
    accept("string literal", { case s: gToken.LitStr => gExpr.gLit.Str(s) })
  }

  private def litArray: Parser[gExpr.gLit.Array] = positioned {
    gToken.BracketL() ~> repsep(blocklike, gToken.Comma()) <~ gToken.BracketR() ^^ (items => gExpr.gLit.Array(items))
  }

  private def litRecord: Parser[gExpr.gLit.Record] = positioned {
    (gToken.BraceL() ~> repsep(identifier ~ gToken.Colon() ~ blocklike, gToken.Comma()) <~ gToken.BraceR()) ^^ (kvpairs => gExpr.gLit.Record(kvpairs.map { case k ~ _ ~ v => k -> v }.toMap))
  }

  def lit: Parser[gExpr.gLit] = positioned {
    litStr
      | accept("float literal", { case f: gToken.LitFloat => gExpr.gLit.Float(f) })
      | accept("int literal", { case i: gToken.LitInt => gExpr.gLit.Int(i) })
      | accept("duration literal", { case td: gToken.LitDuration => gExpr.gLit.Duration(td) })
      | accept("datetime literal", { case td: gToken.LitDateTime => gExpr.gLit.DateTime(td) })
      | litArray
      | litRecord
  }

  private def implicitRef: Parser[gExpr.ImplicitRef] = positioned {
    gToken.Underscore() ^^^ gExpr.ImplicitRef()
  }

  private def assign: Parser[gExpr.Assign] = positioned {
    (member ~ gToken.Equal() ~ blocklike) ^^ { case obj ~ _ ~ value => gExpr.Assign(obj, value) }
      | (implicitRef ~ gToken.Equal() ~ blocklike) ^^ { case obj ~ _ ~ value => gExpr.Assign(obj, value) }
      | (identifier ~ gToken.Equal() ~ blocklike) ^^ { case obj ~ _ ~ value => gExpr.Assign(obj, value) }
  }

}

class gTokenReader(tokens: Seq[gToken]) extends Reader[gToken] {
  override def first: gToken = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[gToken] = new gTokenReader(tokens.tail)
}
