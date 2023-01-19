package flux

case class Print(s: String, singleLine: Boolean = true){
  def ++(o: Print): Print =
    Print(s + o.s, singleLine & o.singleLine)
}



case class PrintContext(indent: Int = 0)

case class Printer(lineLength: Int = 120, indent: String = "\t") {

  def print(pStmt: pStmt): Print = p(pStmt, PrintContext())

  def p(pStmt: pStmt, ctx: PrintContext): Print =
    pStmt match
      case Many(stmts) =>
        val renders = stmts.map(p(_, ctx))
        renders match
          case Nil => Print("")
          case one :: Nil => one
          case _ =>
            if (renders.map(_.s).map(_.length).sum < lineLength) {
              renders.reduceLeft(_ ++ _)
            } else {
              Print(renders.map(_.s).mkString(""), singleLine = false)
            }
      case v: Single => renderOne(v)
      case Parenthesised(stmts, sep, begin, end) =>
        val rs = stmts match
          case Many(stmts) => stmts.map(p(_, ctx))
          case _ => List(p(stmts, ctx))
        val joined = rs.map(_.s).mkString(printOption(sep, ctx).s)
        Print(printOption(begin, ctx).s + joined + printOption(end, ctx).s)
      case s: WhiteSpace =>
        s match
          case WhiteSpace.Newline => Print("\n" + indent * ctx.indent, singleLine = false)
          case WhiteSpace.Space => Print(" ")
      case Indent(stmt, increase) => p(stmt, ctx.copy(indent = ctx.indent + increase))

  private def renderOne(s: Single): Print = {
    Print(s.stmt)
  }

  private def printOption(s: Option[pStmt], ctx: PrintContext): Print =
    s.map(p(_, ctx)).getOrElse(Print(""))
}
