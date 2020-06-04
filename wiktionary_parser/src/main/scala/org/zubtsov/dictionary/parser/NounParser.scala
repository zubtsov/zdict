package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.{Number, PartOfSpeech}
import org.zubtsov.dictionary.html.HTMLDocument
import org.zubtsov.dictionary.html.elemet.{HTMLNounTable, HTMLTable}

import scala.collection.mutable.ListBuffer

class NounParser extends Parser {

  override def parse(htmlDocument: HTMLDocument, word: String): ListBuffer[Lexeme] = {

    val nouns = new ListBuffer[Lexeme]()
    val casesTables = getCasesTable(htmlDocument)

    casesTables.foreach(table => {
      val lexemeForms = ListBuffer[LexemeForm]()
      val rows = table.child.filter(_.child.nonEmpty)
      for (rowNum <- 1 until rows.size) {
        val row = htmlDocument.getElementsFromNodeByTag(rows(rowNum), "td")
        val caseType = getCase(htmlDocument, row.head)
        lexemeForms += getLexemeForm(row(1), Array(caseType, Number.Singular.toString))
        lexemeForms += getLexemeForm(row(2), Array(caseType, Number.Plural.toString))
      }
      nouns += Lexeme(word, PartOfSpeech.Noun.toString, lexemeForms)
    })

    nouns
  }

  override def getTableHeader(): HTMLTable = new HTMLNounTable

}
