package org.zubtsov.dictionary.parser

import org.jsoup.nodes.Element
import org.zubtsov.dictionary.feature.CaseEnum
import org.zubtsov.dictionary.html.HTMLDocument

trait CommonNameParser extends Parser {

  def getCase(htmlDocument: HTMLDocument, caseElement: Element): String = {
    CaseEnum.Case(
     getRowTitle(htmlDocument: HTMLDocument, caseElement: Element))
      .toString()
  }
}