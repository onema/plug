package plug

import java.io.{BufferedInputStream, InputStream}
import java.util.zip.GZIPInputStream

import scala.io.Source

object PublicSuffix {

  object Implicits {

    object FromResource {
      implicit lazy val publicSuffix: PublicSuffix = new ResourcePublicSuffix("/public_suffix_list.dat.gz")
    }

  }
}

trait PublicSuffix {
  def splitOnSuffix(hostname: String): (Option[String],Option[String])
}

class ResourcePublicSuffix(resourcePath: String) extends PublicSuffix {

  case class SuffixNode(children: Map[Char,SuffixNode])

  val suffixes: SuffixNode = {
    val resourceStream = getClass.getResourceAsStream(resourcePath)

    Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(resourceStream)))
      .getLines()
      .filterNot(x => x.isEmpty || x.startsWith("//"))
      .foreach(println)
    SuffixNode(Map.empty)
  }

  override def splitOnSuffix(hostname: String): (Option[String], Option[String]) = ???
}
