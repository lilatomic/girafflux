package flux

import scala.annotation.targetName
import scala.quoted.Expr


sealed trait fExpr {}

object fExpr {
  case class Script(imports: List[ModuleImport], queries: List[Query]) extends fExpr

  case class ModuleImport(module: fLit.Str) extends fExpr

  case class Query(from: From, ops: List[|>]) extends fExpr

  case class From(bucket: fExpr.Identifier) extends fExpr

  @targetName("pipe")
  case class |>(inv: Call) extends fExpr

  case class Call(op: fExpr, args: List[Arg]) extends fExpr // TODO: maybe tighten

  case class Arg(name: fToken, value: fExpr) extends fExpr

  case class Identifier(tok: fToken) extends fExpr

  case class Function(params: List[fToken], body: fExpr) extends fExpr

  case class Op1(op: fToken, a0: fExpr) extends fExpr

  case class Op2(op: fToken, a0: fExpr, a1: fExpr) extends fExpr

  case class Member(obj: fExpr, value: fExpr.Identifier | fLit.Str) extends fExpr

  case class Assign(obj: fExpr, value: fExpr) extends fExpr

  case class Block(exprs: List[fExpr]) extends fExpr

  case class Return(expr: fExpr) extends fExpr

  case class PropertyList(elems: Map[fExpr.Identifier, fExpr]) extends fExpr

  case class WithProperties(identifier: fExpr.Identifier, propertyList: fExpr.PropertyList) extends fExpr
}

sealed trait fLit extends fExpr

object fLit {
  case class Boolean(tok: fToken) extends fLit
  case class Integer(tok: fToken) extends fLit

  case class Float(tok: fToken) extends fLit
  case class Duration(tok: fToken) extends fLit
  case class DateTime(tok: fToken) extends fLit
  case class Str(tok: fToken) extends fLit

  case class Regex(tok: fToken) extends fLit

  case class Array(elems: List[fExpr]) extends fLit

  case class Record(body: fExpr.PropertyList | fExpr.WithProperties) extends fLit

  case class Dict(elems: Map[fExpr, fExpr]) extends fLit
}
