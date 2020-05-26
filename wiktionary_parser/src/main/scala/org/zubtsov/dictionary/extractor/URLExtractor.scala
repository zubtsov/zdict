package org.zubtsov.dictionary.extractor

import scala.xml.{Elem, XML}

object URLExtractor {

  def extractDataFromHTML(path: String): Elem = {
    XML.load(path)
  }
}
