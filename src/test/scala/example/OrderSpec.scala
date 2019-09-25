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

}
