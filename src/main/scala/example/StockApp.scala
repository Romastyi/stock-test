package example

object StockApp extends App {
  Client.saveResults(StockItem.calc(Client.readClientsAsMap, Order.readOrders))
}
