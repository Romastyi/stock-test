package example

import org.scalatest.{FlatSpec, Matchers}

class ClientSpec extends FlatSpec with Matchers {

  import Client._

  "Client.parse(...)" should "parse single line" in {
    parsePhrase(parser, "C1\t1000\t130\t240\t760\t320").get should be (Client("C1", 1000, 130, 240, 760, 320))
    parsePhrase(parser, "C2\t4350\t370\t120\t950\t560").get should be (Client("C2", 4350, 370, 120, 950, 560))
    parsePhrase(parser, "C2\t4350\t370\t120\t950\t" ).successful should be (false)
    parsePhrase(parser, "C2\t4350\t370\t120\t\t560" ).successful should be (false)
    parsePhrase(parser, "\t4350\t370\t120\t950\t560").successful should be (false)
    parsePhrase(parser, "4350\t370\t120\t950\t560"  ).successful should be (false)
  }

}
