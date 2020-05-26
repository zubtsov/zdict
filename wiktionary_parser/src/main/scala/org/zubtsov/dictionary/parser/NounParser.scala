package org.zubtsov.dictionary.parser

import java.text.Normalizer

import org.zubtsov.dictionary.entity.{Lexeme, LexemeForm}

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, NodeSeq}

class NounParser extends Parser {

  override def parse(xmlFile: Elem, word: String): ListBuffer[Lexeme] = {

    var nouns = new ListBuffer[Lexeme]()
    var lexemeForms = Array[LexemeForm]()

    val casesTables = getCasesTable(xmlFile)

    val rows = casesTables.head.child.filter(_.child.nonEmpty)

    for (rowNum <- 1 until rows.size) {
      val row = rows(rowNum) \\ "td"
      val caseType = normalizeString((row.head \\ "a").head.attribute("title").getOrElse("").toString)
      lexemeForms +:= new LexemeForm(normalizeString(row(1).text.trim), Array(caseType, "ед.ч"))
      lexemeForms +:= new LexemeForm(normalizeString(row(2).text.trim), Array(caseType, "мн.ч"))
    }

    nouns += new Lexeme(word, "существительное", lexemeForms)

    nouns
  }

  override def isCasesTable(thNodes: NodeSeq): Boolean = {
    (thNodes.length == 3 && (thNodes.head \\ "a").text.matches(".*падеж.*")
      && (thNodes(1) \\ "a").text.matches(".*ед\\..*\\ч.*")
      && (thNodes(2) \\ "a").text.matches(".*мн\\..*ч\\..*"))
  }

  private def normalizeString(str: String): String = {
    Normalizer.normalize(str, Normalizer.Form.NFD).replaceAll("\\p{M}", "")
  }
}
