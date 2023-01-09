package giraffe

import giraffe.gExpr._

val bucketName = "netdatatsdb/autogen"

class Assemble extends munit.FunSuite {
  test("assemble - select all") {
    val expr = Query(
      From(gLit.Str(gToken.LitStr(bucketName))),
      List()
    )
  }
  test("assemble - time range") {
    val expr = Query(
      From(gLit.Str(gToken.LitStr(bucketName))),
      List(
        gStage.range(start = gLit.Duration(gToken.LitDuration(gToken.LitInt("-1"), gToken.LitTimeUnit("h"))))
      )
    )
  }
}
