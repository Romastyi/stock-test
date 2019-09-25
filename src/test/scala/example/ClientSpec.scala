package example

import org.scalatest.{FlatSpec, Matchers}

class ClientSpec extends FlatSpec with Matchers {

  import Client._

  "Client.parse(...)" should "parse single line" in {
    parsePhrase(parser, "C1\t1000\t130\t240\t760\t320").get should be (Client("C1", 1000, 130, 240, 760, 320))
    parsePhrase(parser, "C2\t4350\t370\t120\t950\t560").get should be (Client("C2", 4350, 370, 120, 950, 560))
    parsePhrase(parser, "C2\t4350\t370\t120\t950\t" ).successful should be (false)
    parsePhrase(parser, "C2\t4350\t370\t120\t\t560" ).successful should be (false)
    parsePhrase(parser, "C2\t4.50\t370\t120\t\t560" ).successful should be (false)
    parsePhrase(parser, "C2\t4350\t070\t120\t\t560" ).successful should be (false)
    parsePhrase(parser, "\t4350\t370\t120\t950\t560").successful should be (false)
    parsePhrase(parser, "4350\t370\t120\t950\t560"  ).successful should be (false)
  }

  "Client.buy(...)" should "not make balance negative" in {
    val client = Client("C1", 1000,  100, 100, 100, 100)

    client.buy(Order.Buy("C1", "A",  100, 5)) should be (Client("C1",  500, 105, 100, 100, 100))
    client.buy(Order.Buy("C1", "B",  100, 5)) should be (Client("C1",  500, 100, 105, 100, 100))
    client.buy(Order.Buy("C1", "C",  100, 5)) should be (Client("C1",  500, 100, 100, 105, 100))
    client.buy(Order.Buy("C1", "D",  100, 5)) should be (Client("C1",  500, 100, 100, 100, 105))
    client.buy(Order.Buy("C1", "E",  100, 5)) should be (Client("C1", 1000, 100, 100, 100, 100))

    client.buy(Order.Buy("C1", "A",  999, 1)) should be (Client("C1",    1, 101, 100, 100, 100))
    client.buy(Order.Buy("C1", "B",  999, 1)) should be (Client("C1",    1, 100, 101, 100, 100))
    client.buy(Order.Buy("C1", "C",  999, 1)) should be (Client("C1",    1, 100, 100, 101, 100))
    client.buy(Order.Buy("C1", "D",  999, 1)) should be (Client("C1",    1, 100, 100, 100, 101))
    client.buy(Order.Buy("C1", "E",  999, 1)) should be (Client("C1", 1000, 100, 100, 100, 100))

    client.buy(Order.Buy("C1", "A",  500, 2)) should be (Client("C1",    0, 102, 100, 100, 100))
    client.buy(Order.Buy("C1", "B",  500, 2)) should be (Client("C1",    0, 100, 102, 100, 100))
    client.buy(Order.Buy("C1", "C",  500, 2)) should be (Client("C1",    0, 100, 100, 102, 100))
    client.buy(Order.Buy("C1", "D",  500, 2)) should be (Client("C1",    0, 100, 100, 100, 102))
    client.buy(Order.Buy("C1", "E",  500, 2)) should be (Client("C1", 1000, 100, 100, 100, 100))

    client.buy(Order.Buy("C1", "A", 1001, 2)) should be (Client("C1", 1000, 100, 100, 100, 100))
    client.buy(Order.Buy("C1", "B", 1001, 2)) should be (Client("C1", 1000, 100, 100, 100, 100))
    client.buy(Order.Buy("C1", "C", 1001, 2)) should be (Client("C1", 1000, 100, 100, 100, 100))
    client.buy(Order.Buy("C1", "D", 1001, 2)) should be (Client("C1", 1000, 100, 100, 100, 100))
    client.buy(Order.Buy("C1", "E", 1001, 2)) should be (Client("C1", 1000, 100, 100, 100, 100))
  }

  "Client.sale(...)" should "not sell more then stock's count" in {
    val client = Client("C2", 1000, 100, 100, 100, 100)

    client.sale(Order.Sale("C2", "A", 300, 10)) should be (Client("C2", 4000,  90, 100, 100, 100))
    client.sale(Order.Sale("C2", "B", 300, 10)) should be (Client("C2", 4000, 100,  90, 100, 100))
    client.sale(Order.Sale("C2", "C", 300, 10)) should be (Client("C2", 4000, 100, 100,  90, 100))
    client.sale(Order.Sale("C2", "D", 300, 10)) should be (Client("C2", 4000, 100, 100, 100,  90))
    client.sale(Order.Sale("C2", "E", 300, 10)) should be (Client("C2", 1000, 100, 100, 100, 100))

    client.sale(Order.Sale("C2", "A", 1, 99)) should be (Client("C2", 1099,   1, 100, 100, 100))
    client.sale(Order.Sale("C2", "B", 1, 99)) should be (Client("C2", 1099, 100,   1, 100, 100))
    client.sale(Order.Sale("C2", "C", 1, 99)) should be (Client("C2", 1099, 100, 100,   1, 100))
    client.sale(Order.Sale("C2", "D", 1, 99)) should be (Client("C2", 1099, 100, 100, 100,   1))
    client.sale(Order.Sale("C2", "E", 1, 99)) should be (Client("C2", 1000, 100, 100, 100, 100))

    client.sale(Order.Sale("C2", "A", 1, 100)) should be (Client("C2", 1100,   0, 100, 100, 100))
    client.sale(Order.Sale("C2", "B", 1, 100)) should be (Client("C2", 1100, 100,   0, 100, 100))
    client.sale(Order.Sale("C2", "C", 1, 100)) should be (Client("C2", 1100, 100, 100,   0, 100))
    client.sale(Order.Sale("C2", "D", 1, 100)) should be (Client("C2", 1100, 100, 100, 100,   0))
    client.sale(Order.Sale("C2", "E", 1, 100)) should be (Client("C2", 1000, 100, 100, 100, 100))

    client.sale(Order.Sale("C2", "A", 1, 101)) should be (Client("C2", 1000, 100, 100, 100, 100))
    client.sale(Order.Sale("C2", "B", 1, 101)) should be (Client("C2", 1000, 100, 100, 100, 100))
    client.sale(Order.Sale("C2", "C", 1, 101)) should be (Client("C2", 1000, 100, 100, 100, 100))
    client.sale(Order.Sale("C2", "D", 1, 101)) should be (Client("C2", 1000, 100, 100, 100, 100))
    client.sale(Order.Sale("C2", "E", 1, 101)) should be (Client("C2", 1000, 100, 100, 100, 100))
  }

}
