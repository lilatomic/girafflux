package giraffe

sealed trait gExpr {}

object gExpr {
  case class Query(from: From, Ops: List[gStage]) extends gExpr

  case class From(bucket: Id) extends gExpr

  case class gFunction(args: List[Identifier], body: gExpr) extends gExpr

  sealed trait gBuiltin extends gExpr

  sealed trait gLit extends gExpr

  case class Identifier(tok: gToken) extends gExpr
}

sealed trait gStage extends gExpr

object gStage {
  case class range(start: gExpr, end: gExpr=gBuiltin.Now) extends gStage

  case class map(fn: gExpr.gFunction) extends gStage

  case class filter(fn: gExpr.gFunction) extends gStage
}

object gBuiltin {
  object Now extends gExpr.gBuiltin
}

object gLit {
  case class Duration(tok: gToken) extends gExpr.gLit

}
