package plug.cookie

trait TestSuffix {

  val suffixes = SuffixNode(Map(
    "com" -> SuffixNode.empty,
    "au" -> SuffixNode(Map(
      "com" -> SuffixNode.empty,
      "org" -> SuffixNode.empty,
      "edu" -> SuffixNode(Map(
        "act" -> SuffixNode.empty,
        "nsw" -> SuffixNode.empty
      ))
    ))
  ))

  implicit val suffixLookup = new PublicSuffix {

    def splitOnSuffix(hostnameParts: List[String]): (List[String], List[String]) = suffixes.splitOnSuffix(hostnameParts)
  }
}
