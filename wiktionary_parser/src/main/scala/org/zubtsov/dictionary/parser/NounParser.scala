package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.html.WiktionaryArticle
import org.zubtsov.dictionary.html.elemet.{HTMLNounTable, HTMLTable}
import org.zubtsov.dictionary.utils.StringFormatter

import scala.collection.mutable.ListBuffer

class NounParser extends Parser {

  override def parse(wiktionaryArticle: WiktionaryArticle, word: String): ListBuffer[Lexeme] = {

    var nouns = new ListBuffer[Lexeme]()
    var lexemeForms = Array[LexemeForm]()

    val casesTables = getCasesTable(wiktionaryArticle)

    val rows = casesTables.head.child.filter(_.child.nonEmpty)

    //todo: handle .getOrElse

    for (rowNum <- 1 until rows.size) {
      val row = wiktionaryArticle.getElementsFromNodeByTag(rows(rowNum), "td")
      val caseType = StringFormatter.normalizeString(wiktionaryArticle.getElementsFromNodeByTag(row.head, "a")
        .head.attribute("title").getOrElse("").toString)
      lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(1).text.trim), Array(caseType, "ед.ч"))
      lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, "мн.ч"))
    }

    nouns += new Lexeme(word, "существительное", lexemeForms)

    nouns
  }

  override def getTableHeader(): HTMLTable = new HTMLNounTable

}
