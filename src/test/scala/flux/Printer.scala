package flux

class PrinterTest extends munit.FunSuite {
  test("Parenthesised single") {
    val printed = Printer().print(
      Parenthesised(
        Single("stmt"), begin = Some(Single("(")), end = Some(Single(")")), sep = Some(Single(","))
      )
    )
    assertEquals(printed, Print("(stmt)"))
  }
  test("Parenthesised many") {
    val printed = Printer().print(
      Parenthesised(
        Many(List(Single("stmt0"), Single("stmt1"))), begin = Some(Single("(")), end = Some(Single(")")), sep = Some(Single(","))
      )
    )
    assertEquals(printed, Print("(stmt0,stmt1)"))
  }
}
