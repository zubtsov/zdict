package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.Lexeme
import org.zubtsov.dictionary.html.WiktionaryArticle
import org.zubtsov.dictionary.html.elemet.HTMLTable

import scala.collection.mutable.ListBuffer
import scala.xml.{Node, NodeSeq}

trait Parser {

  def parse(wiktionaryArticle: WiktionaryArticle, word: String): ListBuffer[Lexeme]

  def getTableHeader(): HTMLTable

  def getCasesTable(wiktionaryArticle: WiktionaryArticle): Seq[Node] = {
    for {
      table <- wiktionaryArticle.getElementsFromArticleByTag("tbody")
      tableHeader = table \\ "tr" \\ "th"
      if isCasesTable(tableHeader)
    } yield table
  }

  def isCasesTable(thNodes: NodeSeq): Boolean = {
    val htmlTableHeader = getTableHeader
    (thNodes.length == htmlTableHeader.headerSize
      && (thNodes.map(thNode => (thNode \\ "a").text) zip htmlTableHeader.header)
      .forall(column => column._1.matches(column._2)))
  }
}
