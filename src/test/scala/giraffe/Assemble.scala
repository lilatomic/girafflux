package giraffe

import giraffe.gExpr._

val bucketName = "netdatatsdb/autogen"

class Assemble extends munit.FunSuite {
  test("assemble - select all") {
    val expr = Query(
      From(gToken(bucketName)),
      List()
    )
  }
  test("assemble - time range") {
    val expr = Query(
      From(gToken(bucketName)),
      List(
        gStage.range(start=gLit.Duration(gToken("-1h")))
      )
    )
  }
}
