package example

import scala.annotation.tailrec

/**
 * Баланс заявок по ценной бумаге
 *
 * @param stockName Название ценной бумаги
 * @param listOfBuyTrx Список неподтвержденных заявок на покупку по этой ценной бумаге
 * @param listOfSaleTrx Список неподтвержденных заявок на продажу по этой ценной бумаге
 */
final case class StockItem(stockName: String, listOfBuyTrx: List[Order.Buy], listOfSaleTrx: List[Order.Sale]) {

  @tailrec
  private def applyBuy(buy: Order.Buy, visitedSales: List[Order.Sale], notVisitedSales: List[Order.Sale], mapOfClients: Client.MapOfClients): (List[Order.Buy], List[Order.Sale], Client.MapOfClients) = {
    if (buy.count > 0) {
      notVisitedSales match {
        case Nil =>
          (buy :: Nil, visitedSales, mapOfClients)
        case sale :: tail => sale.matchOther(buy) match {
          case Order.NoMatch =>
            applyBuy(buy, visitedSales :+ sale, tail, mapOfClients)
          case Order.Match(matched, rest: Order.Sale) =>
            applyBuy(buy.copy(count = 0), visitedSales :+ rest, tail, Client.applyOderToMapOfClients(mapOfClients, matched))
          case Order.Match(matched, rest: Order.Buy) =>
            applyBuy(rest, visitedSales, tail, Client.applyOderToMapOfClients(mapOfClients, matched))
        }
      }
    } else (Nil, visitedSales, mapOfClients)
  }


  /**
   * Применить заявку на покупку по всем непотверждённым заявкам на продажу.
   * Применяем до полного удовлетворения заявки.
   * @param order Заявка на покупку
   * @return Новый баланс
   */
  def applyBuyOnSale(order: Order.Buy, mapOfClients: Client.MapOfClients): (StockItem, Client.MapOfClients) = {
    val (buys, sales, clients) = applyBuy(order, Nil, listOfSaleTrx, mapOfClients)
    (this.copy(listOfBuyTrx = listOfBuyTrx ++ buys.filter(_.count > 0), listOfSaleTrx = sales.filter(_.count > 0)), clients)
  }

  @tailrec
  private def applySale(sale: Order.Sale, visitedBuys: List[Order.Buy], notVisitedBuys: List[Order.Buy], mapOfClients: Client.MapOfClients): (List[Order.Sale], List[Order.Buy], Client.MapOfClients) = {
    if (sale.count > 0) notVisitedBuys match {
      case Nil =>
        (sale :: Nil, visitedBuys, mapOfClients)
      case buy :: tail => buy.matchOther(sale) match {
        case Order.NoMatch =>
          applySale(sale, visitedBuys :+ buy, tail, mapOfClients)
        case Order.Match(matched, rest: Order.Buy) =>
          applySale(sale.copy(count = 0), visitedBuys :+ rest, tail, Client.applyOderToMapOfClients(mapOfClients, matched))
        case Order.Match(matched, rest: Order.Sale) =>
          applySale(rest, visitedBuys, tail, Client.applyOderToMapOfClients(mapOfClients, matched))
      }
    } else (Nil, visitedBuys, mapOfClients)
  }

  def applySaleOnBuy(order: Order.Sale, mapOfClients: Client.MapOfClients): (StockItem, Client.MapOfClients) = {
    val (sales, buys, clients) = applySale(order, Nil, listOfBuyTrx, mapOfClients)
    (this.copy(listOfBuyTrx = buys.filter(_.count > 0), listOfSaleTrx = listOfSaleTrx ++ sales.filter(_.count > 0)), clients)
  }

  def applyOrder(order: Order, mapOfClients: Client.MapOfClients): (StockItem, Client.MapOfClients) = {
    if (order.stockName == stockName) {
      order match {
        case buy : Order.Buy  => applyBuyOnSale(buy, mapOfClients)
        case sale: Order.Sale => applySaleOnBuy(sale, mapOfClients)
      }
    } else (this, mapOfClients)
  }

}

object StockItem {

  type Stock = Map[String, StockItem]

  val empty: Stock = Map(
    "A" -> StockItem("A", Nil, Nil),
    "B" -> StockItem("B", Nil, Nil),
    "C" -> StockItem("C", Nil, Nil),
    "D" -> StockItem("D", Nil, Nil)
  )

  def applyOrderToStock(stock: Stock, mapOfClients: Client.MapOfClients, order: Order): (Stock, Client.MapOfClients) = {
    stock.get(order.stockName) match {
      case Some(item) =>
        //println("trx: " + order)
        //println("stock <<: " + stock.mkString("\n"))
        val (s, m) = item.applyOrder(order, mapOfClients)
        //println("stock >>: " + s)
        //println("clients: " + Client.printMapOfClients(m))
        //println("------------------------------")
        (stock.updated(order.stockName, s), m)
      case None =>
        println(s"Unknown stock '${order.stockName}'.")
        (stock, mapOfClients)
    }
  }

  def calcStock(mapOfClients: Client.MapOfClients, orders: Iterator[Order]): (Stock, Client.MapOfClients) = {
    orders.foldLeft((empty, mapOfClients)) {
      case ((stock, clients), order) if Client.canApplyOrder(clients, order) =>
        applyOrderToStock(stock, clients, order)
      case ((stock, clients), order) =>
        println(s"Order '$order' could not be applied.")
        (stock, clients)
    }
  }

  def calc(mapOfClients: Client.MapOfClients, orders: Iterator[Order]): Client.MapOfClients = {
    calcStock(mapOfClients, orders)._2
  }

}
