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

  /**
   * Сопоставить с другой заявкой
   * @param order Заявка для сопоставления
   * @return Результат сопоставления
   */
  def matchOther(order: Order): Order.MatchResult

  /**
   * Общая стоимость заявки
   */
  final def total: Int = count * price

}

object Order extends CommonParsers {

  sealed trait MatchResult
  case object NoMatch extends MatchResult
  final case class Match(matched: List[Order], rest: Order) extends MatchResult

  /**
   * Продажа.
   */
  final case class Sale(override val clientName: String,
                        override val stockName: String,
                        override val price: Int,
                        override val count: Int) extends Order {

    override def matchOther(order: Order): MatchResult = order match {
      case buy @ Buy(_, buyName, buyPrice, buyCount) if buyName == stockName && buyPrice >= price && buyCount > 0 && buyCount >= count =>
        // Если заявка на продажу меньше заявки на покупку,
        // то подверждаем заявку на продажу и уменьшаем заявку на покупку.
        Match(List(this, buy.copy(count = count)).filter(_.count > 0), buy.copy(count = buyCount - count))
      case buy @ Buy(_, buyName, buyPrice, buyCount) if buyName == stockName && buyPrice >= price && buyCount < count =>
        // Если заявка на продажу больше заявки на покупку,
        // то подверждаем заявку на покупку и уменьшаем заявку на продажу.
        Match(List(buy, this.copy(count = buyCount)).filter(_.count > 0), this.copy(count = count - buyCount))
      case _ =>
        // Нельзя соспоставить с заявкой
        NoMatch
    }

  }

  /**
   * Покупка.
   */
  final case class Buy(override val clientName: String,
                       override val stockName: String,
                       override val price: Int,
                       override val count: Int) extends Order {

    override def matchOther(order: Order): MatchResult = order match {
      case sale @ Sale(_, saleName, salePrice, saleCount) if saleName == stockName && salePrice <= price && saleCount > 0 && saleCount <= count =>
        // Если заявка на продажу меньше заявки на покупку,
        // то подтверждаем заявку на продажу и уменьшаем заявку на прокупку
        Match(List(sale, this.copy(count = saleCount)).filter(_.count > 0), this.copy(count = count - saleCount))
      case sale @ Sale(_, saleName, salePrice, saleCount) if saleName == stockName && salePrice <= price && saleCount > count =>
        // Если заявка на продажу больше заявки на покупку,
        // то подтверждаем заявку на покупку и уменьшаем заявку на продажу
        Match(List(this, sale.copy(count = count)).filter(_.count > 0), sale.copy(count = saleCount - count))
      case _ =>
        // Нельзя соспоставить заявку
        NoMatch
    }

  }

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
