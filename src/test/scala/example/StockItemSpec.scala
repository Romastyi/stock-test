package example

import org.scalatest.{FlatSpec, Matchers}

class StockItemSpec extends FlatSpec with Matchers {

  /**
   * C1   b   A   10  10 (1)
   * C2   b   A   10  10 (2)
   * C3   s   A   10  15 (3)
   */
  "StockItem.calcStock(...)" should "several items with same price" in {
    val clients = Map(
      "C1" -> Client("C1", 100, 0, 0, 0, 0),
      "C2" -> Client("C2", 100, 0, 0, 0, 0),
      "C3" -> Client("C3",  0, 15, 0, 0, 0)
    )
    val orders = List(
      Order.Buy ("C1", "A", 10, 10),
      Order.Buy ("C2", "A", 10, 10),
      Order.Sale("C3", "A", 10, 15)
    )
    StockItem.calcStock(clients, orders.iterator) should be ((
      Map(
        "A" -> StockItem("A", List(Order.Buy("C2", "A", 10, 5)), Nil),
        "B" -> StockItem("B", Nil, Nil),
        "C" -> StockItem("C", Nil, Nil),
        "D" -> StockItem("D", Nil, Nil)
      ),
      Map(
        "C1" -> Client("C1",   0, 10, 0, 0, 0),
        "C2" -> Client("C2",  50,  5, 0, 0, 0),
        "C3" -> Client("C3", 150,  0, 0, 0, 0)
      )
    ))
  }

}
