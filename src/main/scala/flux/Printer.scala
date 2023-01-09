package flux

case class Print(s: String)

object Printer {
  def print(pStmt: pStmt): Print =
    pStmt match
      case Many(stmts) =>
        val renders = stmts.map(print)
        renders match
          case one :: Nil => one
          case _ => Print(renders.map(_.s).mkString("\n"))
      case Single(stmt) => Print(stmt)
      case Parenthesised(stmts, sep, begin, end) =>
        val rs = stmts match
          case Many(stmts) => stmts.map(print)
          case _ => List(print(stmts))
        val joined = rs.mkString(sep.getOrElse(""))
        Print(begin.getOrElse("") + joined + end.getOrElse(""))
}
