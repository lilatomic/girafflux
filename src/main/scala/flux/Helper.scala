package flux

import flux.fExpr.{Call, Function, Op2}

object Helper {
  def andMany(conds: List[fExpr]): fExpr =
    conds match
      case ::(head, Nil) => head
      case ::(head, next) => Ops.and(head, andMany(next))
      case Nil => fLit.Boolean(fToken("true"))


  def filterMany(conds: List[fExpr]): Call = Func.filter(Function(List(fToken("r")),andMany(conds)))
}
