package flux

case class Print(s: String, singleLine: Boolean = true)

case class Printer(lineLength: Int = 120) {
  def print(pStmt: pStmt): Print =
    pStmt match
      case Many(stmts) =>
        val renders = stmts.map(print)
        renders match
          case Nil => Print("")
          case one :: Nil => one
          case _ =>
            if (renders.map(_.s).map(_.length).sum < lineLength){
              Print(renders.map(_.s).mkString(" "))
            } else {
              Print(renders.map(_.s).mkString("\n"), singleLine = false)
            }
      case v: Single => renderOne(v)
      case Parenthesised(stmts, sep, begin, end) =>
        val rs = stmts match
          case Many(stmts) => stmts.map(print)
          case _ => List(print(stmts))
        val joined = rs.map(_.s).mkString(sep.getOrElse(""))
        Print(begin.getOrElse("") + joined + end.getOrElse(""))

  private def renderOne(p: Single): Print = {
    val withLineBreak =
      p.lineBreak match
        case LineBreak.Before => Print("\n" + p.stmt, singleLine = false)
        case LineBreak.After => Print(p.stmt + "\n", singleLine = false)
        case LineBreak.Neutral => Print(p.stmt, singleLine = p.stmt.contains("\n"))
    withLineBreak
  }
}
