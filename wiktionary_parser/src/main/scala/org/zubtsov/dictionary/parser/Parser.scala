package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.CaseEnum
import org.zubtsov.dictionary.html.WiktionaryArticle
import org.zubtsov.dictionary.html.elemet.HTMLTable
import org.zubtsov.dictionary.utils.StringFormatter

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

  def getCaseType(wiktionaryArticle: WiktionaryArticle, caseNode: Node): String = {
    CaseEnum.Case(
      StringFormatter
        .normalizeString(
          wiktionaryArticle.getElementsFromNodeByTag(caseNode, "a")
            .head
            .attribute("title")
            .getOrElse(None)
            .toString))
      .toString()
  }

  def getLexemeForm(node: Node, grammaticalFeatures: Array[String]): LexemeForm ={
    LexemeForm(StringFormatter.normalizeString(node.text.trim), grammaticalFeatures)
  }
}
