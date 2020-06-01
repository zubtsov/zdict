package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.feature.{Animacy, CaseEnum, Gender, Number, PartOfSpeech}
import org.zubtsov.dictionary.html.WiktionaryArticle
import org.zubtsov.dictionary.html.elemet.{HTMLAdjectiveTable, HTMLTable}
import org.zubtsov.dictionary.utils.StringFormatter

import scala.collection.mutable.ListBuffer

class AdjectiveParser extends Parser {

  override def parse(wiktionaryArticle: WiktionaryArticle, word: String) : ListBuffer[Lexeme] = {

    var adjectives = new ListBuffer[Lexeme]()
    var lexemeForms = Array[LexemeForm]()

    val casesTables = getCasesTable(wiktionaryArticle)
    //todo: handle empty header
    val rows = casesTables.head.child.filter(_.child.nonEmpty)

    for (rowNum <- 1 until rows.size) {
      val row = wiktionaryArticle.getElementsFromNodeByTag(rows(rowNum), "td")
      if(row.length == 5){
        val caseType = CaseEnum.Case(StringFormatter.normalizeString((row.head \\ "a").head.attribute("title").getOrElse("").toString)).toString()
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(1).text.trim), Array(caseType, Number.Singular.toString, Gender.Masculine.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, Number.Singular.toString, Gender.Neuter.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(3).text.trim), Array(caseType, Number.Singular.toString, Gender.Feminine.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(4).text.trim), Array(caseType, Number.Plural.toString))
      }
      else if(row.length == 3){
        val caseType = CaseEnum.Case("винительныи").toString()
        val neuterSingular = lexemeForms
          .filter(l => l.grammaticalFeatures(0).matches(caseType)
          && l.grammaticalFeatures(1).matches(Number.Singular.toString)
          && l.grammaticalFeatures(2).matches(Gender.Neuter.toString))
          .head.form
        val feminineSingular = lexemeForms
          .filter(l => l.grammaticalFeatures(0).matches(caseType)
            && l.grammaticalFeatures(1).matches(Number.Singular.toString)
            && l.grammaticalFeatures(2).matches(Gender.Feminine.toString))
          .head.form
        lexemeForms +:= new LexemeForm(neuterSingular, Array(caseType, Number.Singular.toString, Gender.Neuter.toString, Animacy.Inanimate.toString))
        lexemeForms +:= new LexemeForm(feminineSingular, Array(caseType, Number.Singular.toString, Gender.Feminine.toString, Animacy.Inanimate.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(1).text.trim), Array(caseType, Number.Singular.toString, Gender.Masculine.toString, Animacy.Inanimate.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, Number.Plural.toString, Animacy.Inanimate.toString))
      }
      else if(row.length == 6){
        val caseType = CaseEnum.Case(StringFormatter.normalizeString((row.head \\ "a").head.attribute("title").getOrElse("").toString)).toString()
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, Number.Singular.toString, Gender.Masculine.toString, Animacy.Animate.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(3).text.trim), Array(caseType, Number.Singular.toString, Gender.Neuter.toString(), Animacy.Animate.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(4).text.trim), Array(caseType, Number.Singular.toString, Gender.Feminine.toString, Animacy.Animate.toString))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(5).text.trim), Array(caseType, Number.Plural.toString, Animacy.Animate.toString))
      }
    }

    adjectives += new Lexeme(word, PartOfSpeech.Adjective.toString, lexemeForms)

    adjectives
  }

  override def getTableHeader(): HTMLTable = new HTMLAdjectiveTable()

}
