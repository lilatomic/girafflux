package il

import flux.{Func, fExpr, fLit, fToken}
import giraffe.gExpr.{gBuiltin, gStage}
import giraffe.{gExpr, gToken}

import scala.util.parsing.combinator.Parsers

object Transformer {

  def transformProgram(expr: gExpr.Script) = {
    Right(g2f(expr))
  }

  def g2f(g: gExpr.Script): fExpr.Script = fExpr.Script(imports = g.imports.map(g2f), queries = g.queries.map(g2f))

  def g2f(g: gExpr.Id): fToken = fToken(g.tok.s)

  def g2f(g: gExpr.From): fExpr.From = fExpr.From(g2f(g.bucket))

  def g2f(g: gExpr.gLit.Str): fLit.Str = fLit.Str(fToken(g.tok.s))

  def g2f(g: gExpr.gLit.Int): fLit.Integer = fLit.Integer(fToken(g.tok.i))

  def g2f(g: gExpr.gLit.Float): fLit.Float = fLit.Float(fToken(g.tok.f))

  def g2f(g: gExpr.gLit.Duration): fLit.Duration =
    val value = g.tok.value match
      case f: gToken.LitFloat => f.f
      case i: gToken.LitInt => i.i

    fLit.Duration(fToken(s"${value}${g.tok.unit.u}"))

  def g2f(g: gExpr.ModuleImport): fExpr.ModuleImport = fExpr.ModuleImport(g2f(g.module).tok)

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
      case gStage.map(id, expr) => ???
      case gStage.mapMany(id, many) => ???
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
      case gExpr.gLit.Record(items) => ???

  def g2fBlocklike(g: gExpr.blocklike): fExpr =
    g match
      case v: gExpr.Call => g2f(v)
      case v: gExpr.Assign => g2f(v)
      case v: gExpr.Index => ???
      case v: gExpr.Id => fExpr.Identifier(fToken(v.tok.s))
      case v: gExpr.gLit => g2fLit(v)
      case v: gExpr.Block => g2fBlocklike(reduceBlock(v))

  def g2f(g: gExpr.Call): fExpr.Call = fExpr.Call(g2fBlocklike(g.callee), g.args.map(g2f))

  def g2f(g: gExpr.Arg): fExpr.Arg = fExpr.Arg(fToken(g.name.tok.s), g2fBlocklike(g.value))

  def g2f(g: gExpr.blocklike | gExpr.gBuiltin.Now.type): fExpr =
    g match
      case v: gExpr.gBuiltin.Now.type => g2f(v)
      case v: gExpr.blocklike => g2fBlocklike(v)

  def g2f(g: gExpr.gBuiltin.Now.type): fExpr = fLit.Str(fToken("now"))

  def g2f(g: gExpr.Assign): fExpr.Assign = fExpr.Assign(g2fBlocklike(g.obj), g2fBlocklike(g.value))

}

object Helpers {
  def fFilterEqual(attr: String, expr: fExpr) =
    fExpr.|>(
      Func.filter(
        fExpr.Function(
          List(fToken("r")), fExpr.Op2(fToken("=="), fExpr.Index(fExpr.Identifier(fToken("r")), fLit.Str(fToken(attr))), expr)
        )
      )
    )
}
