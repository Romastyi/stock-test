package example

import org.scalatest.{FlatSpec, Matchers}

class OrderSpec extends FlatSpec with Matchers {

  import Order._

  "Order.parse(...)" should "parse single line" in {
    parsePhrase(parser, "C8\tb\tC\t15\t4").get should be (Order.Buy("C8", "C", 15, 4))
    parsePhrase(parser, "C2\ts\tC\t14\t5").get should be (Order.Sale("C2", "C", 14, 5))
    parsePhrase(parser, "C2\ta\tC\t14\t5").successful should be (false)
    parsePhrase(parser, "C2\ts\t-\t14\t5").successful should be (false)
    parsePhrase(parser, "C2\ts\tC\t1.4\t5").successful should be (false)
    parsePhrase(parser, "C2\ts\tC\t14\tb").successful should be (false)
    parsePhrase(parser, "C8\t\tC\t15\t4").successful should be (false)
    parsePhrase(parser, "\ts\tC\t14\t5" ).successful should be (false)
  }

  "Order.matchOrder(...)" should "partial matching" in {
    val buy1 = Order.Buy("C1", "A", 10, 12)
    val sale1 = Order.Sale("C2", "A", 8, 10)
    val match1 = Order.Match(List(Order.Sale("C2", "A", 8, 10), Order.Buy("C1", "A", 10, 10)), Order.Buy("C1", "A", 10, 2))
    buy1.matchOther(sale1) should be (match1)
    sale1.matchOther(buy1) should be (match1)

    val buy2 = Order.Buy("C1", "A", 10, 10)
    val sale2 = Order.Sale("C2", "A", 8, 12)
    val match2 = Order.Match(List(Order.Buy("C1", "A", 10, 10), Order.Sale("C2", "A", 8, 10)), Order.Sale("C2", "A", 8, 2))
    buy2.matchOther(sale2) should be (match2)
    sale2.matchOther(buy2) should be (match2)

    buy1.matchOther(buy2) should be (Order.NoMatch)
    sale1.matchOther(sale2) should be (Order.NoMatch)
  }

}
