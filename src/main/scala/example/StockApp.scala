package example

object StockApp extends App {
  println("--- clients.txt ---")
  Client.readClients.foreach(println)
  println("--- orders.txt ---")
  Order.readOrders.foreach(println)
}
