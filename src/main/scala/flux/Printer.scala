package flux

object Printer {
  //  private def mkIndent(s: String, indent: Int = 0): String = ("\t" * indent) + s

  private def l(l: fToken): String = l.lexeme

  def print(expr: fExpr, indent: Int = 0): List[String] =
    expr match
      case fExpr.Query(from, ops) => print(from, indent) ++ ops.flatMap(print(_, indent + 1))
      case fExpr.From(bucket) => List(s"from(bucket: \"${l(bucket)}\")")
      case fExpr.|>(inv) => List("|>") ++ print(inv)
      case fExpr.Call(op, args) => (s"${l(op)}(" :: args.flatMap(print(_, indent))) :+ ")"
      case fExpr.Arg(name, value) => s"${l(name)}:" :: print(value, indent)
      case fExpr.Identifier(tok) => List(l(tok))
      case v: fExpr.Function => printFunction(v, indent)
      case fExpr.Op1(op, a0) => l(op) :: print(a0, indent)
      case fExpr.Op2(op, a0, a1) => (print(a0, indent) :+ l(op)) ++ print(a1, indent)
      case fExpr.Index(obj, value) => (print(obj, indent) :+ "[") ++ print(value, indent) :+ "]"
      case lit: fLit => printLit(lit, indent)

  private def printFunction(v: fExpr.Function, indent: Int): List[String] =
    s"(${v.params.map(l).mkString(",")}) =>" :: print(v.body, indent)

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
