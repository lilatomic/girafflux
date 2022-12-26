package giraffe

import scala.util.parsing.combinator.RegexParsers

object GLexer extends RegexParsers {
  override def skipWhitespace: Boolean = true

  def identifier: Parser[Id] = {
    "\\p{L}[\\p{L}\\p{N}]*".r ^^ { str => Id(str) }
  }

  // text keywords

  def from = "from" ^^ (_ => From)

  // punctiation keywords
  def underscore = "_" ^^ (_ => Underscore)

  def pipe = "|" ^^ (_ => Pipe)

  def atpersat = "@" ^^ (_ => Atpersat)

  def hash = "#" ^^ (_ => Hash)

  def question = "?" ^^ (_ => Question)

  def bracel = "{" ^^ (_ => BraceL)

  def bracer = "}" ^^ (_ => BraceR)

  def bracketl = "[" ^^ (_ => BracketL)

  def bracketr = "]" ^^ (_ => BracketR)

  def parenl = "(" ^^ (_ => ParenL)

  def parenr = ")" ^^ (_ => ParenR)

  def tokens = {
    phrase(rep1(
      parenr | parenl
        | bracketl | bracketr
        | bracel | bracer
        | question | hash | atpersat | pipe
        | from
        | identifier
    ))
  }

  def lex(code: String) = {
    parse(tokens, code)
  }
}
