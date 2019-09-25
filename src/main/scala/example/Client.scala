package example

/**
 * Запись о клиенте
 * @param name Имя клиента
 * @param balance Баланс клиента по долларам
 * @param A Баланс клиента по ценной бумаге A в штуках
 * @param B Баланс клиента по ценной бумаге B в штуках
 * @param C Баланс клиента по ценной бумаге C в штуках
 * @param D Баланс клиента по ценной бумаге D в штуках
 */
final case class Client(name: String, balance: Int, A: Int, B: Int, C: Int, D: Int) {

  def error[A](order: Order, msg: String): Either[String, A] =
    Left(s"Transaction $order couldn't be applied: $msg")

  private def useBalance(order: Order): Either[String, Client] = {
    if (order.total <= balance) Right(copy(balance = balance - order.total))
    else error(order, s"Not enough balance (need: ${order.total}, actual: $balance).")
  }
  private def addBalance(order: Order): Either[String, Client] = {
    Right(copy(balance = balance + order.total))
  }

  private def getStockByName(stockName: String): Int = {
    stockName match {
      case "A" => A
      case "B" => B
      case "C" => C
      case "D" => D
      case _   => 0
    }
  }
  private def addStockByName(order: Order): Either[String, Client] = {
    order.stockName match {
      case "B" => Right(copy(B = B + order.count))
      case "C" => Right(copy(C = C + order.count))
      case "A" => Right(copy(A = A + order.count))
      case "D" => Right(copy(D = D + order.count))
      case other => error(order, s"Unknown stock name '$other'.")
    }
  }
  private def subStockByName(order: Order): Either[String, Client] = {
    order.stockName match {
      case "A" if A >= order.count => Right(copy(A = A - order.count))
      case "B" if B >= order.count => Right(copy(B = B - order.count))
      case "C" if C >= order.count => Right(copy(C = C - order.count))
      case "D" if D >= order.count => Right(copy(D = D - order.count))
      case _ => error(order, s"Not enough stock '${order.stockName}' count (need: ${order.count}, actual: ${getStockByName(order.stockName)}).")
    }
  }


  def buy(order: Order.Buy): Client = {
    (for {
      step1 <- useBalance(order)
      step2 <- step1.addStockByName(order)
    } yield step2) match {
      case Left(error) => println(error); this
      case Right(result) => result
    }
  }

  def sale(order: Order.Sale): Client = {
    (for {
      step1 <- subStockByName(order)
      step2 <- step1.addBalance(order)
    } yield step2) match {
      case Left(error) => println(error); this
      case Right(result) => result
    }
  }

}

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
    case name ~ balance ~ a ~ b ~ c ~ d => Client(name, balance, a, b, c, d)
  }

  def readClients: Iterator[Client] = parseResource(parser, "clients.txt")

  /**
   * Вычитка клиентов в словарь: имя_клиента -> модель_клиента
   * @return Словарь клиентов
   */
  def readClientsAsMap: Map[String, Client] = readClients.map(i => i.name -> i).toMap

}