package flux

case class Print(s: String, singleLine: Boolean = true)

case class PrintContext()

case class Printer(lineLength: Int = 120) {

  def print(pStmt: pStmt): Print = p(pStmt, PrintContext())

  def p(pStmt: pStmt, ctx: PrintContext): Print =
    pStmt match
      case Many(stmts) =>
        val renders = stmts.map(p(_, ctx))
        renders match
          case Nil => Print("")
          case one :: Nil => one
          case _ =>
            if (renders.map(_.s).map(_.length).sum < lineLength){
              Print(renders.map(_.s).mkString(""))
            } else {
              Print(renders.map(_.s).mkString(""), singleLine = false)
            }
      case v: Single => renderOne(v)
      case Parenthesised(stmts, sep, begin, end) =>
        val rs = stmts match
          case Many(stmts) => stmts.map(p(_, ctx))
          case _ => List(p(stmts, ctx))
        val joined = rs.map(_.s).mkString(sep.getOrElse(""))
        Print(begin.getOrElse("") + joined + end.getOrElse(""))
      case s: WhiteSpace =>
        s match
          case WhiteSpace.Newline => Print("\n", singleLine = false)
          case WhiteSpace.Space => Print(" ")

  private def renderOne(p: Single): Print = {
    Print(p.stmt)
  }
}
