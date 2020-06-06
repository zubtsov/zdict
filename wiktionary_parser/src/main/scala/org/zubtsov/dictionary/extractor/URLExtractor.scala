package org.zubtsov.dictionary.extractor

import org.jsoup.Jsoup
import org.zubtsov.dictionary.html.HTMLDocument

object URLExtractor {

  def extractDataFromHTML(path: String): HTMLDocument = {
    HTMLDocument(Jsoup.connect(path).get())
  }
}
