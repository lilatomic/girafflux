package giraffe

import giraffe.gToken.*

import scala.util.parsing.combinator.RegexParsers


object GLexer extends RegexParsers {
  override def skipWhitespace: Boolean = true

  def litTimeUnit: Parser[LitTimeUnit] = {
    "(y|mo|w|d|h|m|s|ms|us|µs|ns)".r ^^ { LitTimeUnit.apply }
  }

  def litDateTime: Parser[LitDateTime] = {
    val date = "[0-9]{4}-[0-9]{2}-[0-9]{2}".r ^^ { s => s}
    val time = "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]*)?((Z)|([+\\-][0-9]{2}:[0-9]{2}))?".r ^^ (s => s)
    val date_time_lit = date ~ opt("T" ~> time) ^^ { case d ~ t => t match
      case Some(value) => LitDateTime(d + "T" + value)
      case None => LitDateTime(d)
    }
    date_time_lit
  }

  def literal: Parser[Lit] = {
    val litStr = "\"(?:[^\"\\\\]|\\\\.)*\"".r ^^ { s => LitStr(s.substring(1, s.length - 1)) }
    val litFloat = "-?\\p{Nd}+.\\p{Nd}*".r ^^ {s => LitFloat(s)}
    val litInt = "-?\\p{Nd}+".r ^^ {s => LitInt(s)}
    val litDuration = litFloat ~ litTimeUnit ^^ { case v ~ u => LitDuration(v, u)} | litInt ~ litTimeUnit ^^ { case v ~ u => LitDuration(v, u)}
    litDateTime | litDuration | litStr | litFloat | litInt
  }

  def identifier: Parser[Id | Underscore] = {
    "[\\p{L}_][\\p{L}\\p{N}_]*".r ^^ { str => str match
      case "_" => Underscore()
      case _ => gToken.Id(str)
    }
  }

  // text keywords

  def from = "from" ^^ (_ => From())

  def moduleImport = "import" ^^^ ModuleImport()

  // punctiation keywords
  def underscore = "_" ^^^ Underscore()

  def period = "." ^^^ Period()

  def pipe = "|" ^^ (_ => Pipe())

  def atpersat = "@" ^^ (_ => Atpersat())

  def hash = "#" ^^ (_ => Hash())

  def dollar = "$" ^^^ Dollar()

  def percent = "%" ^^^ Percent()

  def question = "?" ^^ (_ => Question())

  def plus = "+" ^^^ Plus()

  def equal = "=" ^^^ Equal()

  def bracel = "{" ^^ (_ => BraceL())

  def bracer = "}" ^^ (_ => BraceR())

  def bracketl = "[" ^^ (_ => BracketL())

  def bracketr = "]" ^^ (_ => BracketR())

  def parenl = "(" ^^ (_ => ParenL())

  def parenr = ")" ^^ (_ => ParenR())

  def comma = "," ^^^ Comma()

  def colon = ":" ^^^ Colon()

  def tokens: GLexer.Parser[List[gToken]] = {
    phrase(rep1(
      comma | colon
        | parenl | parenr
        | bracketl | bracketr
        | bracel | bracer
        | plus | question | percent | dollar | hash | atpersat | pipe
        | from | moduleImport | period
        | equal
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
