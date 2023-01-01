package flux

import flux.fExpr._

object Func {
  def range(start: fExpr): Call = Call(Identifier(fToken("range")), List(Arg(fToken("start"), start)))

  def range(start: fExpr, stop: fExpr): Call = Call(Identifier(fToken("range")), List(Arg(fToken("start"), start), Arg(fToken("stop"), stop)))

  def Yield(): Call = Call(Identifier(fToken("yield")), List())

  def limit(n: fExpr): Call = Call(Identifier(fToken("limit")), List(Arg(fToken("n"), n)))

  def filter(fn: Function): Call = Call(Identifier(fToken("filter")), List(Arg(fToken("fn"), fn)))

  def sort(columns: fExpr, desc: Option[fExpr]): Call = {
    val col = Arg(fToken("columns"), columns)
    desc match
      case Some(value) => Call(Identifier(fToken("sort")), List(col, Arg(fToken("desc"), value)))
      case None => Call(Identifier(fToken("sort")), List(col))
  }
}
