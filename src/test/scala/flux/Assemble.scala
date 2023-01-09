package flux

import flux.fExpr.*
import flux.Func.*
import flux.Helper.filterMany
import flux.Ops

val bucketName = "netdatatsdb/autogen"
val _measurement = fToken("_measurement")

class Assemble extends munit.FunSuite {
  test("assemble - select all") {
    val expr = Query(
      From(fLit.Str(fToken(bucketName))),
      List()
    )
  }
  test("assemble - time range") {
    val expr = Query(
      From(fLit.Str(fToken(bucketName))),
      List(
        |>(Call(Identifier(fToken("range")), args = List(
          Arg(fToken("start"), fLit.Duration(fToken("-1h")))
        )))
      )
    )
    val rendered = Renderer.render(expr)
    val printed = Printer().print(rendered).s
    assertEquals(printed,
      """from(bucket: "netdatatsdb/autogen")
        |> range(start: -1h)""")
  }
  test("assemble - inline lambda") {
    val expr = Query(
      From(fLit.Str(fToken(bucketName))),
      List(
        |>(Call(Identifier(fToken("range")), args = List(
          Arg(fToken("start"), fLit.Duration(fToken("-1h")))
        ))),
        |>(Call(Identifier(fToken("filter")), args = List(
          Arg(
            fToken("fn"),
            Function(
              List(fToken("r")), Op2(fToken("=="), Member(Identifier(fToken("r")), fLit.Str(_measurement)), fLit.Str(fToken("vpsmetrics")))
            )
          )
        )))
      )
    )
    val rendered = Renderer.render(expr)
    val printed = Printer().print(rendered).s
    assertEquals(printed,
      """from(bucket: "netdatatsdb/autogen")
|> range(start: -1h)
|> filter(
fn: (r) =>
r["_measurement"] == "vpsmetrics"
)""")
  }

  test("assemble - with helpers") {
    val expr = Query(
      From(fLit.Str(fToken(bucketName))),
      List(
        |>(range(fLit.Duration(fToken("-1h")))),
        |>(filterMany(List(
          Ops.eq(Member(Identifier(fToken("r")), fLit.Str(_measurement)), fLit.Str(fToken("cpsmetrics"))),
          Ops.eq(fLit.Str(fToken("host")), fLit.Str(fToken("vpsfrsqlpac1"))),
          Ops.eq(Member(Identifier(fToken("r")), fLit.Str(fToken("_field"))), fLit.Str(fToken("pcpu"))),
          Ops.gt(Member(Identifier(fToken("r")), fLit.Str(fToken("_value"))), fLit.Integer(fToken("80"))),
        ))),
        |>(sort(fLit.Array(List(fLit.Str(fToken("_value")))), desc = Some(fLit.Boolean(fToken("true"))))),
        |>(limit(fLit.Integer(fToken("10")))),
        |>(Yield())
      )
    )
    val rendered = Renderer.render(expr)
    val printed = Printer().print(rendered).s
    assertEquals(printed,
      """from(bucket: "netdatatsdb/autogen")
|> range(start: -1h)
|> filter(
fn: (r) =>
r["_measurement"] == "cpsmetrics" and "host" == "vpsfrsqlpac1" and r["_field"] == "pcpu" and r["_value"] > 80
)
|> sort(
columns: ["_value"],
desc: true,
)
|> limit(n: 10)
|> yield()""")
  }
}
