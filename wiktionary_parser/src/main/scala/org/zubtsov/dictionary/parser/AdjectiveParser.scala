package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.{Animacy, Gender, Number, PartOfSpeech}
import org.zubtsov.dictionary.html.HTMLDocument
import org.zubtsov.dictionary.html.elemet.{HTMLAdjectiveTable, HTMLTable}

import scala.collection.mutable.ListBuffer

class AdjectiveParser extends CommonNameParser {

  override def parse(htmlDocument: HTMLDocument, word: String): ListBuffer[Lexeme] = {

    val adjectives = new ListBuffer[Lexeme]()
    val casesTables = getFormsTable(htmlDocument)

    casesTables.foreach(table => {
      val lexemeForms = ListBuffer[LexemeForm]()
      val rows = htmlDocument.getChildElements(table)
      for (rowNum <- 1 until rows.size) {
        val row = htmlDocument.getElementsFromNodeByTag(rows(rowNum), "td")

        row.length match {
          case 5 => {
            val caseType = getCase(htmlDocument, row.head)
            lexemeForms ++= getLexemeForm(row(1), Array(caseType, Number.Singular.toString, Gender.Masculine.toString))
            lexemeForms ++= getLexemeForm(row(2), Array(caseType, Number.Singular.toString, Gender.Neuter.toString))
            lexemeForms ++= getLexemeForm(row(3), Array(caseType, Number.Singular.toString, Gender.Feminine.toString))
            lexemeForms ++= getLexemeForm(row(4), Array(caseType, Number.Plural.toString))
          }
          case 3 => {
            val previousRow = htmlDocument.getElementsFromNodeByTag(rows(rowNum - 1), "td")
            val caseType = getCase(htmlDocument, previousRow.head)
            lexemeForms ++= getLexemeForm(row(1), Array(caseType, Number.Singular.toString, Gender.Masculine.toString, Animacy.Inanimate.toString))
            lexemeForms ++= getLexemeForm(row(2), Array(caseType, Number.Plural.toString, Animacy.Inanimate.toString))
            lexemeForms ++= getLexemeForm(previousRow(3), Array(caseType, Number.Singular.toString, Gender.Neuter.toString, Animacy.Inanimate.toString))
            lexemeForms ++= getLexemeForm(previousRow(4), Array(caseType, Number.Singular.toString, Gender.Feminine.toString, Animacy.Inanimate.toString))
          }
          case 6 => {
            val caseType = getCase(htmlDocument, row.head)
            lexemeForms ++= getLexemeForm(row(2), Array(caseType, Number.Singular.toString, Gender.Masculine.toString, Animacy.Animate.toString))
            lexemeForms ++= getLexemeForm(row(3), Array(caseType, Number.Singular.toString, Gender.Neuter.toString, Animacy.Animate.toString))
            lexemeForms ++= getLexemeForm(row(4), Array(caseType, Number.Singular.toString, Gender.Feminine.toString, Animacy.Animate.toString))
            lexemeForms ++= getLexemeForm(row(5), Array(caseType, Number.Plural.toString, Animacy.Animate.toString))
          }
          //todo: ???
          case _ => ""
        }
      }
      adjectives += Lexeme(word, PartOfSpeech.Adjective.toString, lexemeForms)
    })

    adjectives
  }

  override def getTableHeader(): HTMLTable = new HTMLAdjectiveTable()

}
