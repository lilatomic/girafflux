package il

import flux.{Func, Ops, fExpr, fLit, fToken, mkOp2}
import giraffe.gExpr.{gBuiltin, gStage}
import giraffe.{gExpr, gToken}

import scala.annotation.tailrec
import scala.util.parsing.combinator.Parsers

object Transformer {

  def transformProgram(expr: gExpr.Script): Right[Nothing, fExpr.Script] = {
    Right(g2f(expr))
  }

  def g2f(g: gExpr.Script): fExpr.Script = fExpr.Script(imports = g.imports.map(g2f), queries = g.queries.map(g2f))

  def g2f(g: gExpr.Id): fExpr.Identifier = fExpr.Identifier(fToken(g.tok.s))

  def g2f(g: gExpr.From): fExpr.From = fExpr.From(g2f(g.bucket))

  def g2f(g: gExpr.gLit.Str): fLit.Str = fLit.Str(fToken(g.tok.s))

  def g2f(g: gExpr.gLit.Int): fLit.Integer = fLit.Integer(fToken(g.tok.i))

  def g2f(g: gExpr.gLit.Float): fLit.Float = fLit.Float(fToken(g.tok.f))

  def g2f(g: gExpr.gLit.Duration): fLit.Duration =
    val value = g.tok.value match
      case f: gToken.LitFloat => f.f
      case i: gToken.LitInt => i.i

    fLit.Duration(fToken(s"$value${g.tok.unit.u}"))

  def g2f(g: gExpr.ModuleImport): fExpr.ModuleImport = fExpr.ModuleImport(g2f(g.module))

  def g2f(g: gExpr.Query): fExpr.Query = fExpr.Query(g2f(g.from), g.ops.map(g2f))

  def g2f(g: gExpr.gStage): fExpr.|> =
    g match
      case gStage.range(start, stop) =>
        val argStart = fExpr.Arg(fToken("start"), g2fBlocklike(start))
        val argStop = fExpr.Arg(fToken("stop"), g2f(stop))
        val args = if (stop == gExpr.gBuiltin.Now) {
          List(argStart)
        } else {
          List(argStart, argStop)
        }
        fExpr.|>(fExpr.Call(fExpr.Identifier(fToken("range")), args))
      case v: gStage.map => Helpers.fMap(v)
      case v: gStage.mapMany => Helpers.fMapMany(v)
      case gStage.filter(fn) => ???
      case gStage.filterMeasurement(_measurement) =>
        val measurementExpr = g2fBlocklike(reduceBlock(_measurement))
        Helpers.fFilterEqual("_measurement", measurementExpr)

      case gStage.filterField(_field) =>
        Helpers.fFilterEqual("_field", g2fBlocklike(reduceBlock(_field)))
      case gStage.filterFieldMany(_fields) => ???
      case gStage.streamMap(block) =>
        reduceBlock(block) match
          case v: gExpr.Call => fExpr.|>(g2f(v))

  def reduceBlock(e: gExpr.blocklike): gExpr.blocklike =
    e match
      case b: gExpr.Block =>
        b.exprs match
          case ::(head, Nil) => head
          case _ => b
      case _ => e

  def g2f(g: gExpr.Block): fExpr.Block =
    if (g.exprs.size == 1) {
      val expr = g.exprs.head // TODO: Better unrolling
      fExpr.Block(List(g2fBlocklike(expr)))
    } else {
      fExpr.Block(g.exprs.map(g2fBlocklike))
    }

  def g2fLit(g: gExpr.gLit): fLit =
    g match
      case v: gExpr.gLit.Str => g2f(v)
      case v: gExpr.gLit.Float => g2f(v)
      case v: gExpr.gLit.Int => g2f(v)
      case v: gExpr.gLit.Duration => g2f(v)
      case gExpr.gLit.Array(items) => fLit.Array(items.map(g2fBlocklike))
      case v: gExpr.gLit.Record => fLit.Record(g2fPropertyList(v))

  def g2fPropertyList(g: gExpr.gLit.Record): fExpr.PropertyList =
    fExpr.PropertyList(
      g.items.map((k, v) => fExpr.Identifier(fToken(k.tok.s)) -> g2fBlocklike(v))
    )

  def g2fBlocklike(g: gExpr.blocklike): fExpr =
    g match
      case v: gExpr.Call => g2fCall(v)
      case v: gExpr.Assign => g2f(v)
      case v: gExpr.Member => g2f(v)
      case v: gExpr.Id => fExpr.Identifier(fToken(v.tok.s))
      case v: gExpr.gLit => g2fLit(v)
      case v: gExpr.Block => g2fBlocklike(reduceBlock(v))
      case v: gExpr.ImplicitRef => resolveImplicitRef(v)

  def g2f(g: gExpr.Call): fExpr.Call = fExpr.Call(g2fBlocklike(g.callee), g.args.map(g2f))

  def g2fCall(g: gExpr.Call): fExpr =
    @tailrec
    def getMemberChain(m: gExpr.Member, acc: List[gExpr.Id | gExpr.gLit.Str] = List()): (gExpr.assignable, List[gExpr.Id | gExpr.gLit.Str]) =
      m.obj match
        case v: gExpr.Member => getMemberChain(v, m.value :: acc)
        case _ => (m.obj, m.value :: acc)

    g.callee match
      case member: gExpr.Member =>
        val (head, memberChain) = getMemberChain(member)
        head match
          case gExpr.Id(gToken.Id("math")) => transformMathlike(memberChain, g)
          case gExpr.Id(gToken.Id("cmp")) => transformCmplike(memberChain, g)
          case _ => fExpr.Call(g2fBlocklike(g.callee), g.args.map(g2f))
      case _ => fExpr.Call(g2fBlocklike(g.callee), g.args.map(g2f))

  def mkOp2(s: mkOp2, g: gExpr.Call) =
    s(
      g2fBlocklike(g.args(0).value),
      g2fBlocklike(g.args(1).value)
    )

  def transformMathlike(chain: List[gExpr.Id | gExpr.gLit.Str], g: gExpr.Call): fExpr =
    val s = chain.head match
      case v: gExpr.Id => v.tok.s
      case v: gExpr.gLit.Str => v.tok.s

    val op = s match
      case "add" => Ops.addition
      case "sub" => Ops.substraction
      case "div" => Ops.division
      case "mul" => Ops.multiplication
      case "exp" => Ops.exponentiation
      case "mod" => Ops.modulo
    mkOp2(op, g)

  def transformCmplike(chain: List[gExpr.Id | gExpr.gLit.Str], g: gExpr.Call): fExpr =
    val s = chain.head match
      case v: gExpr.Id => v.tok.s
      case v: gExpr.gLit.Str => v.tok.s

    val op = s match
      case "eq" => Ops.eq
      case "ne" => Ops.ne
      case "lt" => Ops.lt
      case "gt" => Ops.gt
      case "le" => Ops.le
      case "ge" => Ops.ge
      case "regex_eq" => Ops.regex_eq
      case "regex_ne" => Ops.regex_ne
    mkOp2(op, g)

  def g2f(g: gExpr.Arg): fExpr.Arg = fExpr.Arg(fToken(g.name.tok.s), g2fBlocklike(g.value))

  def g2f(g: gExpr.blocklike | gExpr.gBuiltin.Now.type): fExpr =
    g match
      case v: gExpr.gBuiltin.Now.type => g2f(v)
      case v: gExpr.blocklike => g2fBlocklike(v)

  def g2f(g: gExpr.gBuiltin.Now.type): fExpr = fLit.Str(fToken("now"))

  def g2f(g: gExpr.Assign): fExpr.Assign =
    val obj = g.obj match
      case v: gExpr.ImplicitRef => resolveImplicitRef(v)
      case v: gExpr.Member => g2f(v)
      case v: gExpr.Id => g2f(v)
    fExpr.Assign(obj, g2fBlocklike(g.value))

  def g2f(g: gExpr.Member): fExpr.Member =
    fExpr.Member(
      g2f(g.obj),
      g.value match
        case v: gExpr.Id => g2f(v)
        case v: gExpr.gLit.Str => g2f(v)
    )

  def resolveImplicitRef(g: gExpr.ImplicitRef): fExpr =
  // TODO: some actual logic, we won't always be in a map
    fExpr.Identifier(fToken("r"))
}

object Helpers {
  def fFilterEqual(attr: String, expr: fExpr): fExpr.|> =
    fExpr.|>(
      Func.filter(
        fExpr.Function(
          List(fToken("r")), fExpr.Op2(fToken("=="), fExpr.Member(fExpr.Identifier(fToken("r")), fLit.Str(fToken(attr))), expr)
        )
      )
    )


  def fMap(v: gExpr.gStage.map): fExpr.|> =
    val target = v.id match
      case _: gExpr.ImplicitRef => "_value"
      case i: gExpr.Id => i.tok.s
    fExpr.|>(
      Func.map(
        fExpr.Op2(
          fToken("with"),
          fExpr.Identifier(fToken("r")),
          fExpr.Op2(
            fToken(":"),
            fExpr.Identifier(fToken(target)),
            Transformer.g2fBlocklike(v.expr)
          )
        )
      )
    )

  def fMapMany(v: gExpr.gStage.mapMany): fExpr.|> =
    val returnStmt = v.id match
      case None => fExpr.Return(
        fLit.Record(fExpr.WithProperties(
          fExpr.Identifier(fToken("r")),
          Transformer.g2fPropertyList(v.record)
        ))
      )
      case Some(value) => fExpr.Return(
        fLit.Record(fExpr.WithProperties(
          fExpr.Identifier(fToken("r")),
          Transformer.g2fPropertyList(gExpr.gLit.Record(
            Map(gExpr.Id(gToken.Id("_value")) -> v.record)
          )) // creates synthetic `r with _value: {...}`
        ))
      )

    val body = v.block match
      case Some(value) =>
        fExpr.Block(
          Transformer.g2f(value).exprs :+ returnStmt
        )
      case None => returnStmt

    fExpr.|>(
      Func.map(
        body
      )
    )
}
