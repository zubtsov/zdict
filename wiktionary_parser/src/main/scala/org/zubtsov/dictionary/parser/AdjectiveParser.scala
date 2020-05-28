package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}
import org.zubtsov.dictionary.html.WiktionaryArticle
import org.zubtsov.dictionary.html.elemet.{HTMLAdjectiveTable, HTMLTable}
import org.zubtsov.dictionary.utils.StringFormatter

import scala.collection.mutable.ListBuffer

class AdjectiveParser extends Parser {

  override def parse(wiktionaryArticle: WiktionaryArticle, word: String) : ListBuffer[Lexeme] = {

    var adjectives = new ListBuffer[Lexeme]()
    var lexemeForms = Array[LexemeForm]()

    val casesTables = getCasesTable(wiktionaryArticle)

    print(casesTables)
    //todo: handle empty header
    val rows = casesTables.head.child.filter(_.child.nonEmpty)

    var accusativeIntex = 0

    for (rowNum <- 1 until rows.size) {
      val row = rows(rowNum) \\ "td"
      if(row.length == 5){
        val caseType = StringFormatter.normalizeString((row.head \\ "a").head.attribute("title").getOrElse("").toString)
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(1).text.trim), Array(caseType, "ед.ч.", "муж. р."))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, "ед.ч.", "ср. р."))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(3).text.trim), Array(caseType, "ед.ч.", "жен. р."))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(4).text.trim), Array(caseType, "мн.ч."))
      }
      else if(row.length == 3){
        val caseType = "винительный"
        val neuterSingular = lexemeForms
          .filter(l => l.grammaticalFeatures(0).matches(".*винительн.*")
          && l.grammaticalFeatures(1).matches(".*ед\\.ч\\..*")
          && l.grammaticalFeatures(2).matches(".*ср\\.\\s+р\\..*"))
          .head.form
        val feminineSingular = lexemeForms
          .filter(l => l.grammaticalFeatures(0).matches(".*винительн.*")
            && l.grammaticalFeatures(1).matches(".*ед\\.ч\\..*")
            && l.grammaticalFeatures(2).matches(".*жен\\.\\s+р\\..*"))
          .head.form
        lexemeForms +:= new LexemeForm(neuterSingular, Array(caseType, "ед.ч.", "ср. р.", "неодушевленное"))
        lexemeForms +:= new LexemeForm(feminineSingular, Array(caseType, "ед.ч.", "жен. р.", "неодушевленное"))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(1).text.trim), Array(caseType, "ед.ч.", "муж. р.", "неодушевленное"))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, "мн.ч.", "неодушевленное"))
      }
      else if(row.length == 6){
        val caseType = StringFormatter.normalizeString((row.head \\ "a").head.attribute("title").getOrElse("").toString)
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(2).text.trim), Array(caseType, "ед.ч.", "муж. р.", "одушевленное"))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(3).text.trim), Array(caseType, "ед.ч.", "ср. р.", "одушевленное"))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(4).text.trim), Array(caseType, "ед.ч.", "жен. р.", "одушевленное"))
        lexemeForms +:= new LexemeForm(StringFormatter.normalizeString(row(5).text.trim), Array(caseType, "мн.ч.", "одушевленное"))
      }
    }

    adjectives += new Lexeme(word, "прилагательное", lexemeForms)

    adjectives
  }

  override def getTableHeader(): HTMLTable = new HTMLAdjectiveTable()

}
