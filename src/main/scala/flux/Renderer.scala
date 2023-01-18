package flux

import flux.WhiteSpace.{Newline, Space}
import flux.pStmt

import scala.annotation.unused

final class PrintingException(@unused arg: String) extends RuntimeException

object Renderer {

  private def l(l: fToken): Single = Single(l.lexeme)

  private def prependOptionToList(a: Option[pStmt], l: Many): Many =
    a match
      case Some(v) => Many(v :: l.stmts)
      case None => l

  private def appendOptionToList(l: Many, a: Option[pStmt]): Many =
    a match
      case Some(v) => Many(l.stmts :+ v)
      case None => l

  private def prependToFirst(s: String, l: pStmt): pStmt =
    Many(List(Single(s), l))
  //    l match
  //      case ::(head, next) => (s + head) :: next
  //      case Nil => throw PrintingException(s"expected at least 1 item in list")

  private def appendToLast(l: List[String], s: String): List[String] =
    l match
      case Nil => throw PrintingException(s"expected at least 1 item in list")
      case _ => l.init :+ (l.last + s)

  private def parenthesised(open: String, items: Many, separator: String, close: String): Parenthesised =
    Parenthesised(items, begin = Some(open), end = Some(close), sep = None)

  private def coalesceSingles(s0: pStmt, s1: pStmt, sep: String, start: Option[String] = None, end: Option[String] = None): Parenthesised =
    Parenthesised(
      Many(List(s0, s1)),
      sep = Some(sep),
      begin = start,
      end = end
    )
  //
  //    s0 match
  //      case head0 :: Nil =>
  //        s1 match
  //          case head1 :: Nil => List(start.getOrElse("") + head0 + sep + head1 + end.getOrElse(""))
  //          case _ => (start.getOrElse("") + head0 + sep) :: appendOptionToList(s1, end)
  //      case _ =>
  //        s1 match
  //          case head1 :: Nil => prependOptionToList(start, s0) :+ (sep + head1 + end.getOrElse(""))
  //          case _ => prependOptionToList(start, s0) ++ (sep :: appendOptionToList(s1, end))

  private def coalesceSingle(s0: Many, start: Option[String] = None, end: Option[String] = None): pStmt =
    Parenthesised(s0, sep = None, begin = start, end = end)
  //    s0.stmts match
  //      case head0 :: Nil => List(head0, start.getOrElse("") + head0 + end.getOrElse(""))
  //      case _ => appendOptionToList(prependOptionToList(start, s0), end)

  def render(expr: fExpr, indent: Int = 0): pStmt =
    expr match
      case fExpr.Query(from, ops) => render(from) ++ Many(ops.map(render(_)))
      case fExpr.From(bucket) => Single(s"from(bucket: \"${l(bucket.tok).stmt}\")")
      case fExpr.|>(inv) => Newline ++ Single("|>") ++ Space ++ render(inv)
      case fExpr.Call(op, args) =>
        Many(List(render(op), Parenthesised(Many(args.map(render(_))), begin = Some("("), end = Some(")"), sep = Some(","))))
      case fExpr.Arg(name, value) => Single(l(name).stmt) ++ Single(":") ++ Space ++ render(value, indent)
      case fExpr.Identifier(tok) => l(tok)
      case v: fExpr.Function => printFunction(v, indent)
      case fExpr.Op1(op, a0) => l(op) ++ render(a0, indent)
      case v: fExpr.Op2 => printOp2(v, indent)
      case fExpr.Member(obj, value) =>
        value match
          case _: fExpr.Identifier => coalesceSingles(render(obj, indent), render(value, indent), ".")
          case _: fLit.Str => coalesceSingles(render(obj, indent), render(value, indent), "[", end = Some("]"))

      case lit: fLit => printLit(lit, indent)
      case fExpr.Script(imports, queries) =>
          Many(imports.map { i => render(i, indent) })
            ++ Newline
            ++ Many(queries.map(q => render(q, indent)))
      case fExpr.ModuleImport(module) =>
        Single("import") ++ Space ++ render(module) ++ Newline
      case fExpr.Block(exprs) => coalesceSingle(Many(exprs.map(render(_, indent))), start = Some("{"), end = Some("}"))
      case fExpr.Assign(obj, value) => coalesceSingles(
        render(obj), render(value), sep = "="
      )
      case fExpr.Return(body) => prependToFirst("return ", render(body))
      case fExpr.PropertyList(elems) => parenthesised("",
        Many(elems.map {
          (k: fExpr.Identifier, v: fExpr) =>
            coalesceSingles(render(k), render(v), sep = ": ")
        }.toList),
        ",", "")
      case fExpr.WithProperties(identifier, propertyList) =>
        render(identifier) ++ Space ++ Single("with") ++ Space ++ render(propertyList)
  //        coalesceSingles(render(identifier), render(propertyList), sep = " with ")

  private def printFunction(v: fExpr.Function, indent: Int): pStmt =
    Single(s"(${v.params.map(l).map(_.stmt).mkString(",")}) =>") ++ render(v.body, indent)

  private def printOp2(op: fExpr.Op2, indent: Int, space: Boolean = true, parens: Boolean = true): pStmt =
    val printedOp = if (space) Space ++ l(op.op) ++ Space else l(op.op)
    val body = Many(List(render(op.a0, indent), printedOp, render(op.a1, indent)))
    if (parens) Many(List(Single("("), body, Single(")"))) else body

  private def printLit(lit: fLit, indent: Int): pStmt =
    lit match
      case fLit.Boolean(tok) => l(tok)
      case fLit.Integer(tok) => l(tok)
      case fLit.Float(tok) => l(tok)
      case fLit.Duration(tok) => l(tok)
      case fLit.DateTime(tok) => l(tok)
      case fLit.Str(tok) => Single(s"\"${l(tok).stmt}\"")
      case fLit.Regex(tok) => Single(s"/${l(tok).stmt}/")
      case fLit.Array(elems) =>
        parenthesised("[", Many(elems.map(render(_, indent))), ",", "]")
      case fLit.Record(body) => Parenthesised(render(body), begin = Some("{"), end = Some("}"))
      case fLit.Dict(elems) => if (elems.isEmpty) {
        Single("[:]")
      } else {
        parenthesised("[", Many(elems.map { (k, v) => coalesceSingles(render(k), render(v), sep = ":") }.toList), separator = ",", close = "]")
      }
}
