package plug

import java.io.{BufferedInputStream, InputStream}
import java.util.zip.GZIPInputStream

import scala.io.Source

object PublicSuffix {

  object Implicits {

    object FromResource {
      implicit lazy val publicSuffix: PublicSuffix = {
        val resourceStream = getClass.getResourceAsStream("/public_suffix_list.dat.gz")
        new ResourcePublicSuffix(new GZIPInputStream(new BufferedInputStream(resourceStream)))
      }
    }

    // TODO: create FromUri that fetches https://publicsuffix.org/list/public_suffix_list.dat and caches locally
  }

}

trait PublicSuffix {
  def splitOnSuffix(hostnameParts: List[String]): (List[String], List[String])
}


class ResourcePublicSuffix(resourceStream: InputStream) extends PublicSuffix {


  val suffixes: SuffixNode = {

    Source.fromInputStream(resourceStream)
      .getLines()
      .filterNot(x => x.isEmpty || x.startsWith("//"))
      .foldLeft(SuffixNode(Map.empty)) {
        case (n, suffix) => n.addSuffix(suffix.split('.').reverse.toList)
      }
  }

  override def splitOnSuffix(hostnameParts: List[String]): (List[String], List[String]) =
    suffixes.splitOnSuffix(hostnameParts)
}