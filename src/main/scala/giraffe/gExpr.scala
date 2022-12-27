package giraffe

import scala.util.parsing.input.Positional

sealed trait gExpr extends Positional

object gExpr {

  case class Query(from: From, Ops: List[gStage]) extends gExpr

  case class From(bucket: Id) extends gExpr

  case class gFunction(args: List[Id], body: gExpr) extends gExpr

  sealed trait gBuiltin extends gExpr

  sealed trait gLit extends gExpr

  case class Id(tok: gToken) extends gExpr


  sealed trait gStage extends gExpr

  object gStage {
    case class range(start: gExpr, end: gExpr = gBuiltin.Now) extends gStage

    case class map(fn: gFunction) extends gStage

    case class filter(fn: gFunction) extends gStage

    case class aaa(inv: String) extends gStage

    case class bbb(inv: String) extends gStage
  }

  object gBuiltin {
    object Now extends gBuiltin
  }

  object gLit {
    case class Duration(tok: gToken) extends gLit

  }
}
