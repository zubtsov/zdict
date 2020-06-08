package org.zubtsov.dictionary.parser

import org.jsoup.nodes.Element
import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.html.HTMLDocument
import org.zubtsov.dictionary.html.elemet.HTMLTable
import org.zubtsov.dictionary.utils.StringFormatter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Parser {

  def parse(htmlDocument: HTMLDocument, word: String): ListBuffer[Lexeme]

  def getTableHeader(): HTMLTable

  def getFormsTable(htmlDocument: HTMLDocument) = {
    for {
      table <- htmlDocument.getElementsFromArticleByTag("tbody")
      tableHeader = htmlDocument.getTableHeader(table)
      if isFormsTable(htmlDocument, tableHeader)
    } yield table
  }

  def isFormsTable(htmlDocument: HTMLDocument, header: mutable.Buffer[Element]): Boolean = {
    val htmlTableHeader = getTableHeader()
    (header.length == htmlTableHeader.headerSize
      && (header.map(column => column.text()) zip htmlTableHeader.header)
      .forall(column => column._1.matches(column._2)))
  }

  def getRowTitle(htmlDocument: HTMLDocument, caseElement: Element): String = {
    StringFormatter
      .normalizeString(
        htmlDocument.getAttributeValue(htmlDocument.getElementsFromNodeByTag(caseElement, "a").head, "title"))
  }

  def getLexemeForm(element: Element, grammaticalFeatures: Array[String]): ListBuffer[LexemeForm] = {
    val rowValues = element.text.trim.split("\\s+")
    val lexemeForms = ListBuffer[LexemeForm]()
    rowValues.foreach(value => lexemeForms += LexemeForm(StringFormatter.normalizeString(value), grammaticalFeatures :+ getStressIndex(element)))
    lexemeForms
  }

  def getStressIndex(element: Element) = {
    val stressIndex = "\\p{M}".r.findAllMatchIn(element.text.trim).map(_.start).toList.headOption
    if (stressIndex.isDefined) (stressIndex.get - 1).toString
    //todo ???
    else ""
  }
}
