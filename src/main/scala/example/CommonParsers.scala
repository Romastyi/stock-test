package example

import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers {

  /**
   * Поля записи отделяются друг от друга с помощью символа табуляции (\t).
   */
  val delimiter: Parser[Unit] = '\t' ^^ { _ => () }

  /**
   * Имена клиентов, названия ценных бумаг - строки, состоящие из буквенных и цифровых символов ASCII без разделителей.
   */
  val name: Parser[String] = """[a-zA-Z0-9]+""".r ^^ identity

  /**
   * Числовые значения представлены целыми числами.
   */
  val number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }

  def parsePhrase[T](parser: Parser[T], line: String): ParseResult[T] = parse(phrase(parser), line)

  def parseResource[T](parser: Parser[T], resource: String): Iterator[T] = scala.io.Source.fromResource(resource).getLines().zipWithIndex.flatMap {
    case (line, idx) => parsePhrase(parser, line) match {
      case NoSuccess(error, _) =>
        println(s"[$resource] Error parsing line #${idx + 1}: $error")
        None
      case Success(value, _) =>
        Some(value)
    }
  }

}
