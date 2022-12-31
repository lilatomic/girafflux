package giraffe

object WorkFlow {
  def run(code: String): Either[gCompilationError, gExpr] = {
    for {
      tokens <- GLexer.lex(code)
      ast <- GParser.parse(tokens)
    } yield ast
  }
}

class Parser extends munit.FunSuite {
  test("anything") {
    val expr = WorkFlow.run("""from x |._ "zzz" |@ start "y0" stop "y1" |@ start "x0"""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("map with identifier") {
    val expr = WorkFlow.run("from x |. q w")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("common-queries/multiple-fields-in-calculations/") {
    val expr = WorkFlow.run("""from examplebucket |@ start "1m" |% "A" , "B" |._ mathmul(a0: _.A, a1: _.B)""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("map without identifier") {
    val expr = WorkFlow.run("from x |._ w")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("common-queries/operate-on-columns/#find-and-count-unique-values-in-a-column") {
    val expr = WorkFlow.run("""from noaa |@ start "-30d" | group() | keep(columns: ["location"]) | unique(columns: "location")""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("common-queries/operate-on-columns/#recalculate-the-_values-column") {
    val expr = WorkFlow.run("""from noaa |$ "average_temperature" |@ start "-30d" |._ mathdiv(a0: mathmul(a0: mathsub(a0: _, a1: 32.0), a1: 5.0), a1: 9.0)""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("common-queries/operate-on-columns/#calculate-a-new-column"){
    val expr = WorkFlow.run("""from noaa |$ "average_temperature" |@ start "-30d" |. celsius mathdiv(a0: mathmul(a0: mathsub(a0: _, a1: 32.0), a1: 5.0), a1: 9.0)""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("streamMap with call"){
    val expr = WorkFlow.run("from a | q ( v: v )")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }

  test("map call with no identifier"){
    val expr = WorkFlow.run("from a |._ q ( v: v )")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }

  test("map call with identifier") {
    val expr = WorkFlow.run("from a |. b q ( v: v )")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }

  test("map call wtih multiple args") {
    val expr = WorkFlow.run("from a |._ q ( a0: a0 , a1: a1 )")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }

  test("literals")  {
    val expr = WorkFlow.run("from a |._ 12")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("literals - array of ints") {
    val expr = WorkFlow.run("from a |._ [1 , 2 , 3 ]")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("literals - array of strings") {
    val expr = WorkFlow.run("""from a |._ ["s"]""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }

  test("mapWith") {
    val expr = WorkFlow.run("""from a |. q "b"""")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("mapWith with call") {
    val expr = WorkFlow.run("from a |. x q ( a0: v, a1: 32 )")
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("common-queries/iot-common-queries/#calculate-time-in-state") {
    val expr = WorkFlow.run("""import "contrib/tomhollingworth/events" from machine |@ start "2021-08-01T00:00:00Z" stop "2021-08-02T00:30:00Z" |$ "machinery" |% "state" | events.duration( unit: "1h", columnName: "duration" ) | group (columns: ["_value", "_start", "_stop", "station_id"]) | sum(column: "duration") | pivot(rowKey: ["_stop"], columnKey: ["_value"], valueColumn: "duration") """)
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("record"){
    val expr = WorkFlow.run("""from a |. q { w: 1 , e: 2 } """)
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
  test("map many"){
    val expr = WorkFlow.run("""from a |. { w: 1 , e: 2 } """)
    pprint.pprintln(expr)
    assert(expr.isRight)
  }
}
