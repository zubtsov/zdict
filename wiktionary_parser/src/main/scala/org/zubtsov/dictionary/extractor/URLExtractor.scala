package org.zubtsov.dictionary.extractor

import org.zubtsov.dictionary.html.HTMLDocument

import scala.xml.XML

object URLExtractor {

  def extractDataFromHTML(path: String): HTMLDocument = {
    HTMLDocument(XML.load(path))
  }
}
