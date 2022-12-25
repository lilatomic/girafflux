package flux

import flux.fExpr._

object Func {
  def range(start: fExpr): Call = Call(fToken("range"), List(Arg(fToken("start"), start)))

  def range(start: fExpr, stop: fExpr): Call = Call(fToken("range"), List(Arg(fToken("start"), start), Arg(fToken("stop"), stop)))

  def Yield(): Call = Call(fToken("yield"), List())

  def limit(n: fExpr): Call = Call(fToken("limit"), List(Arg(fToken("n"), n)))

  def filter(fn: Function): Call = Call(fToken("filter"), List(Arg(fToken("fn"), fn)))

  def sort(columns: fExpr, desc: Option[fExpr]): Call = {
    val col = Arg(fToken("columns"), columns)
    desc match
      case Some(value) => Call(fToken("sort"), List(col, Arg(fToken("desc"), value)))
      case None => Call(fToken("sort"), List(col))
  }
}
