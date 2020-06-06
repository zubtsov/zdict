package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.{Mood, PartOfSpeech, Tense}
import org.zubtsov.dictionary.html.HTMLDocument
import org.zubtsov.dictionary.html.elemet.{HTMLTable, HTMLVerbTable}

import scala.collection.mutable.ListBuffer

class VerbParser extends Parser {

  override def parse(htmlDocument: HTMLDocument, word: String): ListBuffer[Lexeme] = {

    val verbs = new ListBuffer[Lexeme]()
    val casesTables = getFormsTable(htmlDocument)

    casesTables.foreach(table => {
      val lexemeForms = ListBuffer[LexemeForm]()
      val rows = htmlDocument.getChildElements(table).toList
      for (rowNum <- 1 until rows.size) {
        val row = htmlDocument.getElementsFromNodeByTag(rows(rowNum), "td")

        //todo: add conjugation
        row.length match {
          case 4 => {
            val rowTitle = getRowTitle(htmlDocument, row.head)
            lexemeForms += getLexemeForm(row(1), Array(rowTitle, Tense.Present.toString))
            lexemeForms += getLexemeForm(row(2), Array(rowTitle, Tense.Past.toString))
            lexemeForms += getLexemeForm(row(3), Array(rowTitle, Mood.Imperative.toString))
          }
          case 2 => {
            val rowTitle = getRowTitle(htmlDocument, row.head)
            lexemeForms += getLexemeForm(row(1), Array(rowTitle))
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