package flux

import scala.annotation.targetName

sealed trait pStmt {
  @targetName("concat")
  def ++(o: pStmt): Many = Many(List(this, o))
}

case class Many(stmts: List[pStmt]) extends pStmt

case class Single(stmt: String) extends pStmt

sealed trait WhiteSpace extends pStmt

object WhiteSpace {
  object Newline extends WhiteSpace
  object Space extends WhiteSpace

  def combine(l: WhiteSpace, r: WhiteSpace): WhiteSpace=
    (l,r) match
      case (_, Newline) => Newline
      case (Newline, _) => Newline
      case (Space, Space) => Space
}

case class Parenthesised(stmts: pStmt, sep: Option[String]=None, begin: Option[String]=None, end: Option[String]=None) extends pStmt {}
