package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.{Number, PartOfSpeech}
import org.zubtsov.dictionary.html.WiktionaryArticle
import org.zubtsov.dictionary.html.elemet.{HTMLNounTable, HTMLTable}

import scala.collection.mutable.ListBuffer

class NounParser extends Parser {

  override def parse(wiktionaryArticle: WiktionaryArticle, word: String): ListBuffer[Lexeme] = {

    var nouns = new ListBuffer[Lexeme]()
    var lexemeForms = Array[LexemeForm]()

    val casesTables = getCasesTable(wiktionaryArticle)

    val rows = casesTables.head.child.filter(_.child.nonEmpty)

    for (rowNum <- 1 until rows.size) {
      val row = wiktionaryArticle.getElementsFromNodeByTag(rows(rowNum), "td")
      val caseType = getCaseType(wiktionaryArticle, row.head)
      lexemeForms +:= getLexemeForm(row(1), Array(caseType, Number.Singular.toString))
      lexemeForms +:= getLexemeForm(row(2), Array(caseType, Number.Plural.toString))
    }

    nouns += Lexeme(word, PartOfSpeech.Noun.toString, lexemeForms)

    nouns
  }

  override def getTableHeader(): HTMLTable = new HTMLNounTable

}
