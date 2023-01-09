package flux

import scala.annotation.targetName

enum LineBreak:
  case Before, After, Neutral

sealed trait pStmt {
  @targetName("concat")
  def ++(o: pStmt): Many = Many(List(this, o))
}

case class Many(stmts: List[pStmt]) extends pStmt

case class Single(stmt: String, lineBreak: LineBreak = LineBreak.Neutral) extends pStmt

case class Parenthesised(stmts: pStmt, sep: Option[String]=None, begin: Option[String]=None, end: Option[String]=None) extends pStmt {}
