package giraffe

import giraffe.gToken.*

import scala.util.parsing.combinator.RegexParsers


object GLexer extends RegexParsers {
  override def skipWhitespace: Boolean = true

  def literal: Parser[Lit] = {
    "\"(?:[^\"\\\\]|\\\\.)*\"".r ^^ { s => Lit(s.substring(1, s.length - 1)) }
  }

  def identifier: Parser[Id] = {
    "\\p{L}[\\p{L}\\p{N}]*".r ^^ { str => Id(str) }
  }

  // text keywords

  def from = "from" ^^ (_ => From())

  // punctiation keywords
  def underscore = "_" ^^ (_ => Underscore())

  def period = "." ^^^ Period()

  def pipe = "|" ^^ (_ => Pipe())

  def atpersat = "@" ^^ (_ => Atpersat())

  def hash = "#" ^^ (_ => Hash())

  def dollar = "$" ^^^ Dollar()

  def percent = "%" ^^^ Percent()

  def question = "?" ^^ (_ => Question())

  def bracel = "{" ^^ (_ => BraceL())

  def bracer = "}" ^^ (_ => BraceR())

  def bracketl = "[" ^^ (_ => BracketL())

  def bracketr = "]" ^^ (_ => BracketR())

  def parenl = "(" ^^ (_ => ParenL())

  def parenr = ")" ^^ (_ => ParenR())

  def comma = "," ^^^ Comma()

  def tokens: GLexer.Parser[List[gToken]] = {
    phrase(rep1(
      comma
        | parenl | parenr
        | bracketl | bracketr
        | bracel | bracer
        | parenl | parenr
        | question | percent | dollar | hash | atpersat | pipe
        | from | period
        | literal
        | identifier
        | underscore
    )) ^^ { rawTokens => rawTokens }
  }

  def lex(code: String): Either[gLexerError, List[gToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(gLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }
}
