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
      From(fToken(bucketName)),
      List()
    )
  }
  test("assemble - time range") {
    val expr = Query(
      From(fToken(bucketName)),
      List(
        |>(Call(fToken("range"), args = List(
          Arg(fToken("start"), fLit.Duration(fToken("-1h")))
        )))
      )
    )
  }
  test("assemble - inline lambda") {
    val expr = Query(
      From(fToken(bucketName)),
      List(
        |>(Call(fToken("range"), args = List(
          Arg(fToken("start"), fLit.Duration(fToken("-1h")))
        ))),
        |>(Call(fToken("filter"), args = List(
          Arg(
            fToken("fn"),
            Function(
              List(fToken("r")), Op2(fToken("=="), Index(Identifier(fToken("r")), fLit.Duration(_measurement)), fLit.Duration(fToken("vpsmetrics")))
            )
          )
        )))
      )
    )
  }
}

class Helpers extends munit.FunSuite {

  test("assemble - with helpers"){
    val expr = Query(
      From(fToken(bucketName)),
      List(
        |>(range(fLit.Duration(fToken("-1h")))),
        |>(filterMany(List(
          Ops.eq(Index(Identifier(fToken("r")), fLit.Str(_measurement)), fLit.Str(fToken("cpsmetrics"))),
          Ops.eq(fLit.Str(fToken("host")), fLit.Str(fToken("vpsfrsqlpac1"))),
          Ops.eq(Index(Identifier(fToken("r")), fLit.Str(fToken("_field"))), fLit.Str(fToken("pcpu"))),
          Ops.gt(Index(Identifier(fToken("r")), fLit.Str(fToken("_value"))), fLit.Integer(fToken("80"))),
        ))),
        |>(sort(fLit.Array(List(fLit.Str(fToken("_value")))), desc=Some(fLit.Boolean(fToken("true"))))),
        |>(limit(fLit.Integer(fToken("10")))),
        |>(Yield())
      )
    )
  }
}