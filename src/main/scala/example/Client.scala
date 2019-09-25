package example

/**
 * Запись о клиенте
 * @param name Имя клиента
 * @param dollar Баланс клиента по долларам
 * @param A Баланс клиента по ценной бумаге A в штуках
 * @param B Баланс клиента по ценной бумаге B в штуках
 * @param C Баланс клиента по ценной бумаге C в штуках
 * @param D Баланс клиента по ценной бумаге D в штуках
 */
final case class Client(name: String, dollar: Int, A: Int, B: Int, C: Int, D: Int)

object Client extends CommonParsers {

  /**
   * Файл списка клиетов (clients.txt) имеет следующие поля:
   *
   * - Имя клиента
   * - Баланс клиента по долларам
   * - Баланс клиента по ценной бумаге A в штуках
   * - Баланс по ценной бумаге B
   * - Баланс по ценной бумаге C
   * - Баланс по ценной бумаге D
   *
   * @return Модель клиента
   */
  def parser: Parser[Client] = (name ~ (delimiter ~> number) ~ (delimiter ~> number) ~ (delimiter ~> number) ~ (delimiter ~> number) ~ (delimiter ~> number)) ^^ {
    case name ~ dollar ~ a ~ b ~ c ~ d => Client(name, dollar, a, b, c, d)
  }

  def readClients: Iterator[Client] = parseResource(parser, "clients.txt")

}