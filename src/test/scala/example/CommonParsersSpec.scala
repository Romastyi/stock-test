package example

import org.scalatest.{FlatSpec, Matchers}

class CommonParsersSpec extends FlatSpec with Matchers with CommonParsers {

  "delimiter" should "parse only '\\t' symbol" in {
    parsePhrase(delimiter, "\t").successful should be (true)
    parsePhrase(delimiter, "\n").successful should be (false)
    parsePhrase(delimiter, "t" ).successful should be (false)
  }

  "name" should "parse by regex [a-zA-Z0-9]+" in {
    parsePhrase(name, "C1" ).get should be ("C1")
    parsePhrase(name, "C2" ).get should be ("C2")
    parsePhrase(name, "C3" ).get should be ("C3")
    parsePhrase(name, "C 1").successful should be (false)
    parsePhrase(name, "1"  ).get should be ("1")
    parsePhrase(name, "C"  ).get should be ("C")
    // Russian letter 'С'
    parsePhrase(name, "С1" ).successful should be (false)
  }

  "number" should "parse only Int" in {
    parsePhrase(number, "0"  ).get should be (0)
    parsePhrase(number, "00" ).successful should be (false)
    parsePhrase(number, "01" ).successful should be (false)
    parsePhrase(number, "120").get should be (120)
    parsePhrase(number, "1.3").successful should be (false)
  }

}
