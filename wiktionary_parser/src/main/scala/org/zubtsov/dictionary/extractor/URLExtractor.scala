package org.zubtsov.dictionary.extractor

import org.zubtsov.dictionary.html.WiktionaryArticle

import scala.xml.XML

object URLExtractor {

  def extractDataFromHTML(path: String): WiktionaryArticle = {
    WiktionaryArticle(XML.load(path))
  }
}
