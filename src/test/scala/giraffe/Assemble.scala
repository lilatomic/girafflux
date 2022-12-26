package giraffe

import giraffe.gExpr._

val bucketName = "netdatatsdb/autogen"

class Assemble extends munit.FunSuite {
  test("assemble - select all") {
    val expr = Query(
      From(Id(bucketName)),
      List()
    )
  }
  test("assemble - time range") {
    val expr = Query(
      From(Id(bucketName)),
      List(
        gStage.range(start = gLit.Duration(Lit("-1h")))
      )
    )
  }
}
