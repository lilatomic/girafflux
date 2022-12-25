package flux

import flux.fExpr.{Call, Function, Lit, Op2}

object Helper {
  def andMany(conds: List[fExpr]): fExpr =
    conds match
      case ::(head, Nil) => head
      case ::(head, next) => Op2(fToken("and"), head, andMany(next))
      case Nil => Lit(fToken("true"))


  def filterMany(conds: List[fExpr]): Call = Func.filter(Function(List(fToken("r")),andMany(conds)))
}
