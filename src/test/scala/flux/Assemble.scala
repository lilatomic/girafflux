package flux

import flux.fExpr.*
import flux.Func.*
import flux.Helper.filterMany

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
          Arg(fToken("start"), Lit(fToken("-1h")))
        )))
      )
    )
  }
  test("assemble - inline lambda") {
    val expr = Query(
      From(fToken(bucketName)),
      List(
        |>(Call(fToken("range"), args = List(
          Arg(fToken("start"), Lit(fToken("-1h")))
        ))),
        |>(Call(fToken("filter"), args = List(
          Arg(
            fToken("fn"),
            Function(
              List(fToken("r")), Op2(fToken("=="), Index(Identifier(fToken("r")), Lit(_measurement)), Lit(fToken("vpsmetrics")))
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
        |>(range(Lit(fToken("-1h")))),
        |>(filterMany(List(
          Op2(fToken("=="), Index(Lit(fToken("r")), Lit(_measurement)), Lit(fToken("cpsmetrics"))),
          Op2(fToken("=="), Lit(fToken("host")), Lit(fToken("vpsfrsqlpac1"))),
          Op2(fToken("=="), Index(Lit(fToken("r")), Lit(fToken("_field"))), Lit(fToken("pcpu"))),
          Op2(fToken(">"), Index(Lit(fToken("r")), Lit(fToken("_value"))), Lit(fToken("80"))),
        ))),
        |>(sort(Lit(fToken("[\"_value\"]")), Some(Lit(fToken("true"))))),
        |>(limit(Lit(fToken("10")))),
        |>(Yield())
      )
    )
  }
}