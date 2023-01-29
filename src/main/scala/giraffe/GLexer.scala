package giraffe

import giraffe.gToken.*

import scala.util.parsing.combinator.RegexParsers


object GLexer extends RegexParsers {
  override def skipWhitespace: Boolean = true

  private def litTimeUnit: Parser[LitTimeUnit] = {
    "(y|mo|w|d|h|m|s|ms|us|µs|ns)".r ^^ { LitTimeUnit.apply }
  }

  private def litDateTime: Parser[LitDateTime] = {
    val date = "[0-9]{4}-[0-9]{2}-[0-9]{2}".r ^^ { s => s}
    val time = "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]*)?((Z)|([+\\-][0-9]{2}:[0-9]{2}))?".r ^^ (s => s)
    val date_time_lit = date ~ opt("T" ~> time) ^^ { case d ~ t => t match
      case Some(value) => LitDateTime(d + "T" + value)
      case None => LitDateTime(d)
    }
    date_time_lit
  }

  private def literal: Parser[Lit] = {
    val litStr = "\"(?:[^\"\\\\]|\\\\.)*\"".r ^^ { s => LitStr(s.substring(1, s.length - 1)) }
    val litFloat = "-?\\p{Nd}+\\.\\p{Nd}*".r ^^ {s => LitFloat(s)}
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

  def from: GLexer.Parser[From] = "from" ^^ (_ => From())

  def moduleImport: GLexer.Parser[ModuleImport] = "import" ^^^ ModuleImport()

  // punctuation keywords
  private def underscore = "_" ^^^ Underscore()

  private def period = "." ^^^ Period()

  private def pipe = "|" ^^ (_ => Pipe())

  private def atpersat = "@" ^^ (_ => Atpersat())

  private def hash = "#" ^^ (_ => Hash())

  private def dollar = "$" ^^^ Dollar()

  private def percent = "%" ^^^ Percent()

  private def question = "?" ^^ (_ => Question())

  private def plus = "+" ^^^ Plus()

  private def equal = "=" ^^^ Equal()

  private def bracel = "{" ^^ (_ => BraceL())

  private def bracer = "}" ^^ (_ => BraceR())

  private def bracketl = "[" ^^ (_ => BracketL())

  private def bracketr = "]" ^^ (_ => BracketR())

  private def parenl = "(" ^^ (_ => ParenL())

  private def parenr = ")" ^^ (_ => ParenR())

  private def comma = "," ^^^ Comma()

  private def colon = ":" ^^^ Colon()

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
    def error(msg: String, next: Input): Left[gLexerError, Nothing] = Left(gLexerError(Location(next.pos.line, next.pos.column), msg))
    parse(tokens, code) match {
      case NoSuccess(msg, next) => error(msg, next)
      case Success(result, _) => Right(result)
      case Failure(msg, next) => error(msg, next)
      case Error(msg, next) => error(msg, next)
    }
  }
}
