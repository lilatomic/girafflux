package flux

import scala.annotation.tailrec

case class Print(s: String, singleLine: Boolean = true){
  def ++(o: Print): Print =
    Print(s + o.s, singleLine & o.singleLine)
}

object Print {
  @tailrec
  def combine(ps: List[Print], acc: Print = Print("")): Print =
    ps match
      case ::(head, next) => combine(next, acc ++ head)
      case Nil => acc
}


private def intersperse(stmts: List[pStmt], sep: pStmt): List[pStmt] =
  stmts match
    case Nil => stmts
    case _ :: Nil => stmts
    case x :: xs => (x ++ sep) :: intersperse(xs, sep)


case class PrintContext(indent: Int = 0)

case class Printer(lineLength: Int = 120, indent: String = "\t") {

  def print(pStmt: pStmt): Print = p(simplify(pStmt, PrintContext()), PrintContext())

//  private def combineSimple(stmts: List[pStmt])

  def simplify(pStmt: pStmt, ctx: PrintContext): pStmt =
    pStmt match
      case Many(stmts) =>
        val simplified = stmts.map(simplify(_, ctx))
        Many(simplified)

      case Single(stmt) => Single(stmt)
      case Indent(stmt, increase) => Indent(simplify(stmt, ctx), increase)
      case space: WhiteSpace => space
      case Parenthesised(stmts, sep, begin, end) =>
        val separated = stmts match
          case Many(ss) => sep.map(intersperse(ss, _)).getOrElse(ss)
          case _ => List(stmts)
        val simplified = Many(separated.map(simplify(_, ctx)))
        val joined = p(simplified, ctx)
        if (shouldSplit(joined)) {
          sOption(begin, ctx) ++ Indent(WhiteSpace.Newline ++ simplified) ++ WhiteSpace.Newline ++ sOption(end, ctx)
        } else {
          sOption(begin, ctx) ++ simplified ++ sOption(end, ctx)
        }

  def p(pStmt: pStmt, ctx: PrintContext): Print =
    pStmt match
      case Many(stmts) =>
        val renders = stmts.map(p(_, ctx))
        renders match
          case Nil => Print("")
          case one :: Nil => one
          case _ =>
            if (shouldSplit(renders) & !isMultiline(renders)) {
              Print(
                stmts.map(WhiteSpace.Newline ++ _).map(p(_, ctx)).map(_.s).mkString(""), singleLine = false
              )
            } else {
              Print.combine(renders)
            }
      case v: Single => renderOne(v)
      case Parenthesised(stmts, sep, begin, end) =>
        val rs = stmts match
          case Many(stmts) =>
            sep.map(intersperse(stmts, _)).getOrElse(stmts)
          case _ => List(stmts)
        val joined = rs.map(p(_, ctx))
        printOption(begin, ctx) ++ Print.combine(joined) ++ printOption(end, ctx)

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

  private def sOption(s: Option[pStmt], ctx: PrintContext): pStmt =
    s.map(simplify(_, ctx)).getOrElse(Single(""))

  private def shouldSplit(ps: List[Print]): Boolean =
    ps match
      case Nil => false
//      case x :: Nil => !x.singleLine
      case x :: Nil => false
      case _ :: _ =>
//        ps.exists(!_.singleLine) | (ps.map(_.s).map(_.length).sum >= lineLength)
        (ps.map(_.s).map(_.length).sum >= lineLength)

  private def shouldSplit(p: Print): Boolean =
    p.s.length >= lineLength

  private def isMultiline(ps: List[Print]): Boolean =
    ps.exists(!_.singleLine)
}
