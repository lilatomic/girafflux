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
    Parenthesised(items, begin = Some(Single(open)), end = Some(Single(close)), sep = Some(Single(separator)))

  def render(expr: fExpr): pStmt =
    expr match
      case fExpr.Query(from, ops) => render(from) ++ Many(ops.map((expr: fExpr.|>) => render(expr)))
      case fExpr.From(bucket) => Single(s"from(bucket: \"${l(bucket.tok).stmt}\")")
      case fExpr.|>(inv) => Indent(Newline ++ Single("|>") ++ Space ++ render(inv))
      case fExpr.Call(op, args) =>
        Many(List(render(op), Parenthesised(Many(args.map((expr: fExpr.Arg) => render(expr))), begin = Some(Single("(")), end = Some(Single(")")), sep = Some(Single(",")))))
      case fExpr.Arg(name, value) => Single(l(name).stmt) ++ Single(":") ++ Space ++ render(value)
      case fExpr.Identifier(tok) => l(tok)
      case v: fExpr.Function => printFunction(v)
      case fExpr.Op1(op, a0) => l(op) ++ render(a0)
      case v: fExpr.Op2 => printOp2(v)
      case fExpr.Member(obj, value) =>
        value match
          case _: fExpr.Identifier => Parenthesised(
            Many(List(render(obj), render(value))),
            sep = Some(Single(".")),
          )
          case _: fLit.Str =>
            render(obj) ++ Single("[") ++ render(value) ++ Single("]")
      case lit: fLit => printLit(lit)
      case fExpr.Script(imports, queries) =>
        Many(imports.map { i => render(i) })
          ++ Newline
          ++ Many(queries.map(q => render(q)))
      case fExpr.ModuleImport(module) =>
        Single("import") ++ Space ++ render(module) ++ Newline
      case fExpr.Block(exprs) =>
        Parenthesised(
          Many(exprs.map((expr: fExpr) => render(expr))),
          sep = Some(Newline), begin = Some(Single("{")), end = Some(Single("}"))
        )
      case fExpr.Assign(obj, value) => Parenthesised(
        Many(List(render(obj), render(value))),
        sep = Some(Single("=")),
      )
      case fExpr.Return(body) => prependToFirst("return ", render(body))
      case fExpr.PropertyList(elems) =>
        Parenthesised(
          Many(elems.map {
            (k: fExpr.Identifier, v: fExpr) =>
              Parenthesised(
                Many(List(render(k), render(v))),
                sep = Some(Single(":") ++ Space),
              )
          }.toList),
          sep = Some(Single(",") ++ Space)
        )
      case fExpr.WithProperties(identifier, propertyList) =>
        render(identifier) ++ Space ++ Single("with") ++ Space ++ render(propertyList)
  //        coalesceSingles(render(identifier), render(propertyList), sep = " with ")

  private def printFunction(v: fExpr.Function) =
    val argList = Parenthesised(Many(v.params.map(l).map(_.stmt).map {
      Single.apply
    }), begin = Some(Single("(")), end = Some(Single(")")), sep = Some(Single(",") ++ Space))
    argList ++ Space ++ Single("=>") ++ Space ++ render(v.body)

  private def printOp2(op: fExpr.Op2, space: Boolean = true, parens: Boolean = true) =
    val printedOp = if (space) Space ++ l(op.op) ++ Space else l(op.op)
    val body = Many(List(render(op.a0), printedOp, render(op.a1)))
    if (parens) Many(List(Single("("), body, Single(")"))) else body

  private def printLit(lit: fLit) =
    lit match
      case fLit.Boolean(tok) => l(tok)
      case fLit.Integer(tok) => l(tok)
      case fLit.Float(tok) => l(tok)
      case fLit.Duration(tok) => l(tok)
      case fLit.DateTime(tok) => l(tok)
      case fLit.Str(tok) => Single(s"\"${l(tok).stmt}\"")
      case fLit.Regex(tok) => Single(s"/${l(tok).stmt}/")
      case fLit.Array(elems) =>
        parenthesised("[", Many(elems.map((expr: fExpr) => render(expr))), ", ", "]")
      case fLit.Record(body) => Parenthesised(render(body), begin = Some(Single("{")), end = Some(Single("}")))
      case fLit.Dict(elems) => if (elems.isEmpty) {
        Single("[:]")
      } else {
        parenthesised("[", Many(elems.map { (k, v) =>
          Parenthesised(
            Many(List(render(k), render(v))),
            sep = Some(Single(":")),
          )
        }.toList), separator = ",", close = "]")
      }
}
