package giraffe

sealed trait gCompilationError

case class gLexerError(location: Location, msg: String) extends gCompilationError

case class gParseError(location: Location, msg: String) extends gCompilationError

case class Location(line: Int, column: Int) {
  override def toString: String = s"$line:$column"
}
