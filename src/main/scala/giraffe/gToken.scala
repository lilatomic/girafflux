package giraffe

import scala.util.parsing.input.Positional

sealed trait gToken extends Positional

object gToken {
  // Indentifiers and references
  case class Id(s: String) extends gToken

  case class Underscore() extends gToken

  // Literals
  sealed trait Lit extends  gToken
  case class LitStr(s: String) extends Lit
  case class LitFloat(f: String) extends Lit

  // Keywords

  case class From() extends gToken

  case class Period() extends gToken

  // Stages
  case class Pipe() extends gToken

  case class Atpersat() extends gToken

  case class Hash() extends gToken

  case class Dollar() extends gToken

  case class Percent() extends gToken

  case class Question() extends gToken

  case class Plus() extends gToken

  // Parentheses
  case class BraceL() extends gToken

  case class BraceR() extends gToken

  case class BracketL() extends gToken

  case class BracketR() extends gToken

  case class ParenL() extends gToken

  case class ParenR() extends gToken

  case class Comma() extends gToken
}
