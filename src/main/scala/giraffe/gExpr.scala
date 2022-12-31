package giraffe

import giraffe.gToken.LitStr
import zio.Config.Optional

import scala.util.parsing.input.Positional

sealed trait gExpr extends Positional

object gExpr {

  case class Script(imports: List[ModuleImport], queries: List[Query]) extends gExpr

  case class Block(expr: gExpr) extends gExpr

  object Block {
    def lift(expr: gExpr): Block = Block(expr)
  }

  case class Query(from: From, Ops: List[gStage]) extends gExpr

  case class From(bucket: Id) extends gExpr

  case class ModuleImport(module: gLit.Str, as: Option[gLit.Str] = None) extends gExpr

  case class gFunction(args: List[Id], body: gExpr) extends gExpr

  case class Call(callee: gExpr, args: List[Arg]) extends gExpr

  case class Arg(name: Id, value: gExpr) extends gExpr

  case class Index(obj: gExpr, value: gExpr) extends gExpr

  sealed trait gBuiltin extends gExpr

  sealed trait gLit extends gExpr

  case class Id(tok: gToken) extends gExpr

  case class ImplicitRef() extends gExpr

  sealed trait gStage extends gExpr

  object gStage {
    case class range(start: gExpr, stop: gExpr = gBuiltin.Now) extends gStage

    case class map(id: Id | ImplicitRef, expr: gExpr) extends gStage

    case class mapMany(id: Option[ImplicitRef], many: gLit.Record) extends gStage

    case class filter(fn: gFunction) extends gStage

    case class filterMeasurement(_measurement: gExpr) extends gStage

    case class filterField(_field: gExpr) extends gStage

    case class filterFieldMany(_fields: List[gExpr]) extends gStage

    case class streamMap(call: Call) extends gStage
  }

  object gBuiltin {
    object Now extends gBuiltin
  }

  object gLit {
    case class Duration(tok: gToken) extends gLit

    case class Str(tok: gToken.LitStr) extends gLit

    case class Float(tok: gToken.LitFloat) extends gLit

    case class Int(tok: gToken.LitInt) extends gLit

    case class Array(items: List[gExpr]) extends gLit

    case class Record(items: Map[Id, gExpr]) extends gLit
  }
}
