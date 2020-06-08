package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.{PartOfSpeech, TenseEnum}
import org.zubtsov.dictionary.html.HTMLDocument
import org.zubtsov.dictionary.html.elemet.{HTMLTable, HTMLVerbTable}

import scala.collection.mutable.ListBuffer

class VerbParser extends Parser {

  override def parse(htmlDocument: HTMLDocument, word: String): ListBuffer[Lexeme] = {

    val verbs = new ListBuffer[Lexeme]()
    val formsTables = getFormsTable(htmlDocument)


    formsTables.foreach(table => {
      val lexemeForms = ListBuffer[LexemeForm]()
      val tableHeader = htmlDocument.getTableHeader(table)
      val rows = htmlDocument.getChildElements(table).toList
      for (rowNum <- 1 until rows.size) {
        val row = htmlDocument.getElementsFromNodeByTag(rows(rowNum), "td")

        //todo: add conjugation
        row.length match {
          case 4 => {
            val rowTitle = getRowTitle(htmlDocument, row.head)
            lexemeForms ++= getLexemeForm(row(1), Array(rowTitle, TenseEnum.Tense(tableHeader(0).text()).toString))
            lexemeForms ++= getLexemeForm(row(2), Array(rowTitle, TenseEnum.Tense(tableHeader(1).text()).toString))
            lexemeForms ++= getLexemeForm(row(3), Array(rowTitle, TenseEnum.Tense(tableHeader(2).text()).toString()))
          }
          case 2 => {
            val rowTitle = getRowTitle(htmlDocument, row.head)
            lexemeForms ++= getLexemeForm(row(1), Array(rowTitle))
          }
          //todo: ???
          case _ => ""
        }
      }
      verbs += Lexeme(word, PartOfSpeech.Verb.toString, lexemeForms)
    })

    verbs
  }

  override def getTableHeader(): HTMLTable = new HTMLVerbTable

}