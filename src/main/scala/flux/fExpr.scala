package flux


sealed trait fExpr {}

object fExpr {

  case class Query(from: From, ops: List[|>]) extends fExpr

  case class From(bucket: fToken) extends fExpr

  case class |>(inv: Call) extends fExpr

  case class Call(op: fToken, args: List[Arg]) extends fExpr

  case class Arg(name: fToken, value: fExpr) extends fExpr

  case class Lit(tok: fToken) extends fExpr

  case class Identifier(tok: fToken) extends fExpr

  case class Function(params: List[fToken], body: fExpr) extends fExpr

  case class Op1(op: fToken, a0: fExpr) extends fExpr

  case class Op2(op: fToken, a0: fExpr, a1: fExpr) extends fExpr

  case class Index(obj: fExpr, value: fExpr) extends fExpr
}
