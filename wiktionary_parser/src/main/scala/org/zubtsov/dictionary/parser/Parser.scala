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
      tableHeader = wiktionaryArticle.getElementsFromNodeByTag(table, Seq("tr", "th"))
      if isCasesTable(wiktionaryArticle, tableHeader)
    } yield table
  }

  def isCasesTable(wiktionaryArticle: WiktionaryArticle, header: NodeSeq): Boolean = {
    val htmlTableHeader = getTableHeader()
    (header.length == htmlTableHeader.headerSize
      && (header.map(thNode =>
      wiktionaryArticle.getElementsFromNodeByTag(thNode, "a").text) zip htmlTableHeader.header)
      .forall(column => column._1.matches(column._2)))
  }
}
