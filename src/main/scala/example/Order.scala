package example

sealed trait Order {

  /**
   * Имя клиента выставившего заявку
   */
  def clientName: String

  /**
   * Наименование ценной бумаги
   */
  def stockName: String

  /**
   * Цена заявки (целое число за одну штуку ценной бумаги)
   */
  def price: Int

  /**
   * Количество продаваемых или покупаемых ценных бумаг
   */
  def count: Int

}

object Order extends CommonParsers {

  /**
   * Продажа.
   */
  final case class Sale(override val clientName: String,
                        override val stockName: String,
                        override val price: Int,
                        override val count: Int) extends Order

  /**
   * Покупка.
   */
  final case class Buy(override val clientName: String,
                       override val stockName: String,
                       override val price: Int,
                       override val count: Int) extends Order

  private val SALE_MARK = "s"
  private val BUY_MARK  = "b"
  private val operation: Parser[String] = SALE_MARK | BUY_MARK

  /**
   * Файл списка заявок (orders.txt) имеет формат:
   *
   * - Имя клиента выставившего заявку
   * - Символ операции: s - продажа или b - покупка.
   * - Наименование ценной бумаги
   * - Цена заявки (целое число за одну штуку ценной бумаги)
   * - Количество продаваемых или покупаемых ценных бумаг
   *
   * @return Модель заявки
   */
  def parser: Parser[Order] = (name ~ (delimiter ~> operation) ~ (delimiter ~> name) ~ (delimiter ~> number) ~ (delimiter ~> number)) ^^ {
    case clientName ~ SALE_MARK ~ stockName ~ price ~ count => Sale(clientName, stockName, price, count)
    case clientName ~ BUY_MARK  ~ stockName ~ price ~ count => Buy (clientName, stockName, price, count)
  }

  def readOrders: Iterator[Order] = parseResource(parser, "orders.txt")

}
