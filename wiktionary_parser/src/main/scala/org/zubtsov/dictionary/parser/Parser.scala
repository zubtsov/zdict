package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.CaseEnum
import org.zubtsov.dictionary.html.HTMLDocument
import org.zubtsov.dictionary.html.elemet.HTMLTable
import org.zubtsov.dictionary.utils.StringFormatter

import scala.collection.mutable.ListBuffer
import scala.xml.{Node, NodeSeq}

trait Parser {

  def parse(htmlDocument: HTMLDocument, word: String): ListBuffer[Lexeme]

  def getTableHeader(): HTMLTable

  def getCasesTable(htmlDocument: HTMLDocument): Seq[Node] = {
    for {
      table <- htmlDocument.getElementsFromArticleByTag("tbody")
      tableHeader = htmlDocument.getElementsFromNodeByTag(table, Seq("tr", "th"))
      if isCasesTable(htmlDocument, tableHeader)
    } yield table
  }

  def isCasesTable(htmlDocument: HTMLDocument, header: NodeSeq): Boolean = {
    val htmlTableHeader = getTableHeader()
    (header.length == htmlTableHeader.headerSize
      && (header.map(thNode =>
      htmlDocument.getElementsFromNodeByTag(thNode, "a").text) zip htmlTableHeader.header)
      .forall(column => column._1.matches(column._2)))
  }

  def getCase(htmlDocument: HTMLDocument, caseNode: Node): String = {
    CaseEnum.Case(
      StringFormatter
        .normalizeString(
          htmlDocument.getElementsFromNodeByTag(caseNode, "a")
            .head
            .attribute("title")
            .getOrElse(None)
            .toString))
      .toString()
  }

  def getLexemeForm(node: Node, grammaticalFeatures: Array[String]): LexemeForm = {
    LexemeForm(StringFormatter.normalizeString(node.text.trim), grammaticalFeatures :+ getStressIndex(node))
  }

  def getStressIndex(node: Node) = {
    val stressIndex = "\\p{M}".r.findAllMatchIn(node.text.trim).map(_.start).toList.headOption
    if(stressIndex.isDefined) (stressIndex.get - 1).toString
      //todo ???
    else ""
  }
}
