package giraffe

import giraffe.gToken.LitStr
import zio.Config.Optional

import scala.util.parsing.input.Positional

sealed trait gExpr extends Positional

object gExpr {

  case class Script(imports: List[ModuleImport], queries: List[Query]) extends gExpr

  sealed trait blocklike extends gExpr

  case class Block(exprs: List[blocklike]) extends blocklike

  object Block {
    def lift(expr: blocklike): Block = Block(List(expr))
  }

  case class Query(from: From, ops: List[gStage]) extends gExpr

  case class From(bucket: gLit.Str) extends gExpr

  case class ModuleImport(module: gLit.Str, as: Option[gLit.Str] = None) extends gExpr

  case class gFunction(args: List[Id], body: gExpr) extends gExpr

  case class Call(callee: blocklike, args: List[Arg]) extends blocklike

  case class Arg(name: Id, value: blocklike) extends gExpr

  case class Member(obj: assignable, value: Id | gLit.Str) extends blocklike

  sealed trait gBuiltin extends gExpr

  sealed trait gLit extends blocklike

  case class Id(tok: gToken.Id) extends blocklike

  case class ImplicitRef() extends blocklike

  type assignable = Id | Member | ImplicitRef

  case class Assign(obj: assignable, value: blocklike) extends blocklike

  sealed trait gStage extends gExpr

  object gStage {
    case class range(start: blocklike, stop: blocklike | gBuiltin.Now.type = gBuiltin.Now) extends gStage

    case class map(id: Id | ImplicitRef, expr: blocklike) extends gStage

    case class mapMany(id: Option[ImplicitRef], block: Option[Block], record: gLit.Record) extends gStage

    case class filter(predicate: blocklike) extends gStage

    case class filterMeasurement(_measurement: blocklike) extends gStage

    case class filterField(_field: blocklike) extends gStage

    case class filterFieldMany(_fields: List[gExpr]) extends gStage

    case class streamMap(block: blocklike) extends gStage
  }

  object gBuiltin {
    object Now extends gBuiltin
  }

  object gLit {
    case class Str(tok: gToken.LitStr) extends gLit

    case class Float(tok: gToken.LitFloat) extends gLit

    case class Int(tok: gToken.LitInt) extends gLit

    case class Duration(tok: gToken.LitDuration) extends gLit

    case class DateTime(tok: gToken.LitDateTime) extends gLit

    case class Array(items: List[blocklike]) extends gLit

    case class Record(items: Map[Id, blocklike]) extends gLit
  }
}
