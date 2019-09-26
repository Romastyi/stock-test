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

  override def toString: String = s"$name\t$balance\t$A\t$B\t$C\t$D"

  def error[A](order: Order, msg: String): Either[String, A] =
    Left(s"Transaction $order couldn't be applied: $msg")

  def canUseBalance(order: Order.Buy): Boolean = order.total <= balance

  private def useBalance(order: Order.Buy): Either[String, Client] = {
    if (canUseBalance(order)) Right(copy(balance = balance - order.total))
    else error(order, s"Not enough balance (need: ${order.total}, actual: $balance).")
  }
  private def addBalance(order: Order.Sale): Either[String, Client] = {
    Right(copy(balance = balance + order.total))
  }

  def canUseStock(order: Order.Sale): Boolean = order.count <= getStockByName(order.stockName)

  private def getStockByName(stockName: String): Int = {
    stockName match {
      case "A" => A
      case "B" => B
      case "C" => C
      case "D" => D
      case _   => 0
    }
  }
  private def addStockByName(order: Order.Buy): Either[String, Client] = {
    order.stockName match {
      case "B" => Right(copy(B = B + order.count))
      case "C" => Right(copy(C = C + order.count))
      case "A" => Right(copy(A = A + order.count))
      case "D" => Right(copy(D = D + order.count))
      case other => error(order, s"Unknown stock name '$other'.")
    }
  }
  private def subStockByName(order: Order.Sale): Either[String, Client] = {
    if (canUseStock(order)) order.stockName match {
      case "A" => Right(copy(A = A - order.count))
      case "B" => Right(copy(B = B - order.count))
      case "C" => Right(copy(C = C - order.count))
      case "D" => Right(copy(D = D - order.count))
      case other => error(order, s"Unknown stock name ('$other').")
    } else error(order, s"Not enough stock '${order.stockName}' count (need: ${order.count}, actual: ${getStockByName(order.stockName)}).")
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

  def applyOrder(order: Order): Client = order match {
    case buyOrder: Order.Buy   => buy(buyOrder)
    case saleOrder: Order.Sale => sale(saleOrder)
  }

}

object Client extends CommonParsers {

  type MapOfClients = Map[String, Client]

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
  def readClientsAsMap: MapOfClients = readClients.map(i => i.name -> i).toMap

  def applyOderToMapOfClients(mapOfClients: MapOfClients, orders: List[Order]): MapOfClients = {
    orders.foldLeft(mapOfClients) { case (acc, order) =>
      acc.updatedWith(order.clientName)(_.map(_.applyOrder(order)))
    }
  }

  def canApplyOrder(mapOfClients: MapOfClients, order: Order): Boolean = {
    (mapOfClients.get(order.clientName), order) match {
      case (Some(client), buy : Order.Buy ) => client.canUseBalance(buy)
      case (Some(client), sale: Order.Sale) => client.canUseStock(sale)
      case _ => false
    }
  }

  def printMapOfClients(mapOfClients: MapOfClients): String = {
    mapOfClients.values.toList.sortBy(_.name).mkString("\n")
  }

  def saveResults(mapOfClients: MapOfClients): Unit = {
    val resultsTxt = java.nio.file.Paths.get("results.txt")
    java.nio.file.Files.write(resultsTxt, printMapOfClients(mapOfClients).getBytes)
    println("Write result to file: " + resultsTxt.toAbsolutePath.toString)
  }

}