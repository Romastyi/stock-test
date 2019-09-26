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

  /**
   * C8	b	C	15	4
   * C2	s	C	14	5
   * C2	s	C	13	2
   */
  it should "orders.txt sample" in {
    val clients = Map(
      "C2" -> Client("C2", 100, 0, 0, 7, 0),
      "C8" -> Client("C8", 100, 0, 0, 0, 0)
    )
    val orders = List(
      Order.Buy ("C8", "C", 15, 4),
      Order.Sale("C2", "C", 14, 5),
      Order.Sale("C2", "C", 13, 2)
    )
    StockItem.calcStock(clients, orders.iterator) should be ((
      Map(
        "A" -> StockItem("A", Nil, Nil),
        "B" -> StockItem("B", Nil, Nil),
        "C" -> StockItem("C", Nil, List(Order.Sale("C2", "C", 14, 1), Order.Sale("C2", "C", 13, 2))),
        "D" -> StockItem("D", Nil, Nil)
      ),
      Map(
        "C2" -> Client("C2", 156, 0, 0, 3, 0),
        "C8" -> Client("C8",  40, 0, 0, 4, 0)
      )
    ))

  }

}
