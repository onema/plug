package plug

import java.io.BufferedInputStream

import org.scalatest.{FlatSpec, Matchers, Spec}

/**
  * Created by arne on 1/10/17.
  */
class ResourcePublicSuffixSpec extends FlatSpec with Matchers {

  val resourceStream = getClass.getResourceAsStream("/test_suffix_list.dat")
  val resourceSuffix = new ResourcePublicSuffix(new BufferedInputStream(resourceStream))


  "ctor" should "initialize from input stream" in {
    val expected = SuffixNode(Map(
      "au" -> SuffixNode(Map(
        "com" -> SuffixNode.empty,
        "net" -> SuffixNode.empty,
        "org" -> SuffixNode.empty,
        "edu" -> SuffixNode(Map(
          "act" -> SuffixNode.empty,
          "nsw" -> SuffixNode.empty,
          "nt" -> SuffixNode.empty,
          "qld" -> SuffixNode.empty
        ))
      )),
      "com" -> SuffixNode.empty,
      "edu" -> SuffixNode.empty,
      "org" -> SuffixNode.empty,
      "pg" -> SuffixNode(Map("*" -> SuffixNode.empty)),
      "мон" -> SuffixNode.empty,
      "澳門" -> SuffixNode.empty,
      "澳门" -> SuffixNode.empty,
      "مليسيا" -> SuffixNode.empty,
      "apple" -> SuffixNode.empty,
      "aquarelle" -> SuffixNode.empty,
      "arab" -> SuffixNode.empty
    ))

    resourceSuffix.suffixes should equal(expected)

  }

  "splitOnSuffix" should "split on .مليسيا" in {
    resourceSuffix.splitOnSuffix(List("مليسيا", "مسي")) should equal(List("مسي"), List("مليسيا"))
  }

  it should "split on 澳門" in {
    resourceSuffix.splitOnSuffix(List("澳門", "门")) should equal(List("门"), List("澳門"))
  }
}
