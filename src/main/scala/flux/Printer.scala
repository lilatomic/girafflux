package flux

final class PrintingException(arg: String) extends RuntimeException

object Printer {

  private def l(l: fToken): String = l.lexeme

  private def single(l: List[String]): String =
    l match
      case head :: Nil => head
      case _ => throw PrintingException(s"expected 1 item in list, got ${l.size}")

  def prependOptionToList(a: Option[String], l: List[String]): List[String] =
    a match
      case Some(v) => v :: l
      case None => l

  def appendOptionToList(l: List[String], a: Option[String]): List[String] =
    a match
      case Some(v) => l :+ v
      case None => l

  def prependToFirst(s: String, l: List[String]): List[String] =
    l match
      case ::(head, next) => (s + head) :: next
      case Nil => throw PrintingException(s"expected at least 1 item in list")

  def appendToLast(l: List[String], s: String): List[String] =
    l match
      case Nil => throw PrintingException(s"expected at least 1 item in list")
      case _ => l.init :+ s

  private def coalesceSingles(s0: List[String], s1: List[String], sep: String, start: Option[String] = None, end: Option[String] = None): List[String] =
    s0 match
      case head0 :: Nil =>
        s1 match
          case head1 :: Nil => List(start.getOrElse("") + head0 + sep + head1 + end.getOrElse(""))
          case _ => (start.getOrElse("") + head0 + sep) :: appendOptionToList(s1, end)
      case _ =>
        s1 match
          case head1 :: Nil => prependOptionToList(start, s0) :+ (sep + head1 + end.getOrElse(""))
          case _ => prependOptionToList(start, s0) ++ (sep :: appendOptionToList(s1, end))

  private def coalesceSingle(s0: List[String], start: Option[String] = None, end: Option[String] = None): List[String] =
    s0 match
      case head0 :: Nil => List(start.getOrElse("") + head0 + end.getOrElse(""))
      case _ => appendOptionToList(prependOptionToList(start, s0), end)

  def print(expr: fExpr, indent: Int = 0): List[String] =
    expr match
      case fExpr.Query(from, ops) => print(from, indent) ++ ops.flatMap(print(_, indent + 1))
      case fExpr.From(bucket) => List(s"from(bucket: \"${l(bucket)}\")")
      case fExpr.|>(inv) => prependToFirst("|> ", print(inv))
      case fExpr.Call(op, args) =>
        val opStr = s"${l(op)}("
        val argsLists = args.map(print(_, indent))
        argsLists match
          case Nil => List(opStr + ")")
          case head :: Nil => coalesceSingle(head, start=Some(opStr), end=Some(")"))
          case _ => (opStr :: argsLists.flatMap(x => x :+ ",")) :+ ")"
      case fExpr.Arg(name, value) => prependToFirst(s"${l(name)}: ", print(value, indent))
      case fExpr.Identifier(tok) => List(l(tok))
      case v: fExpr.Function => printFunction(v, indent)
      case fExpr.Op1(op, a0) => l(op) :: print(a0, indent)
      case v: fExpr.Op2 => printOp2(v, indent)
      case fExpr.Index(obj, value) =>
        val objStr = print(obj, indent)
        val valueStr = print(value, indent)
        coalesceSingles(
          objStr, valueStr, "[", end = Some("]")
        )
      case lit: fLit => printLit(lit, indent)

  private def printFunction(v: fExpr.Function, indent: Int): List[String] =
    s"(${v.params.map(l).mkString(",")}) =>" :: print(v.body, indent)

  private def printOp2(op: fExpr.Op2, indent: Int): List[String] =
    coalesceSingles(print(op.a0, indent), print(op.a1, indent), s" ${l(op.op)} ")

  private def printLit(lit: fLit, indent: Int): List[String] =
    lit match
      case fLit.Boolean(tok) => List(l(tok))
      case fLit.Integer(tok) => List(l(tok))
      case fLit.Float(tok) => List(l(tok))
      case fLit.Duration(tok) => List(l(tok))
      case fLit.DateTime(tok) => List(l(tok))
      case fLit.Str(tok) => List(s"\"${l(tok)}\"")
      case fLit.Regex(tok) => List(s"/${l(tok)}/")
      case fLit.Array(elems) => ("[" :: elems.map(print(_, indent)).flatMap(_ :+ ",")) :+ "]"
      case fLit.Record(elems) => ???
      case fLit.Dict(elems) => ???
}
