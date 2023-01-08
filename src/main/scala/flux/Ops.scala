package flux

import flux.fExpr.{Op1, Op2}


def bindOp1(op: String) = (a0: fExpr) => Op1(fToken(op), a0)
def bindOp2(op: String) = (a0: fExpr, a1: fExpr) => Op2(fToken(op), a0, a1)

type mkOp2 = (fExpr, fExpr) => fExpr.Op2

object Ops {
  //  Arithmetic
  val addition: (fExpr, fExpr) => Op2 = bindOp2("+")
  val substraction: (fExpr, fExpr) => Op2 = bindOp2("-")
  val multiplication: (fExpr, fExpr) => Op2 = bindOp2("*")
  val division: (fExpr, fExpr) => Op2 = bindOp2("/")
  val exponentiation: (fExpr, fExpr) => Op2 = bindOp2("^")
  val modulo: (fExpr, fExpr) => Op2 = bindOp2("%")
  // Comparison
  val eq: (fExpr, fExpr) => Op2 = bindOp2("==")
  val ne: (fExpr, fExpr) => Op2 = bindOp2("!=")
  val lt: (fExpr, fExpr) => Op2 = bindOp2("<")
  val gt: (fExpr, fExpr) => Op2 = bindOp2(">")
  val le: (fExpr, fExpr) => Op2 = bindOp2("<=")
  val ge: (fExpr, fExpr) => Op2 = bindOp2(">=")
  val regex_eq: (fExpr, fExpr) => Op2 = bindOp2("=~")
  val regex_ne: (fExpr, fExpr) => Op2 = bindOp2("!~")
  val exists: fExpr => Op1 = bindOp1("exists")
  val and: (fExpr, fExpr) => Op2 = bindOp2("and")
  val or: (fExpr, fExpr) => Op2 = bindOp2("or")

}
