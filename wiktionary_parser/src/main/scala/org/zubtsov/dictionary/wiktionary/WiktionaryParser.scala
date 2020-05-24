package org.zubtsov.dictionary.wiktionary

import scala.io.Source
import scala.xml.{NodeSeq, XML}

//todo: add multithreading
object WiktionaryParser {

  def parse(filepath: String): Unit = {
    Source.fromFile(filepath, "Windows-1251").getLines()
      .filter(_.trim.nonEmpty)
      .map(line => line.split("\\s+")(0))
      .foreach(extractData(_))
  }

  def extractData(word: String): Unit = {
    try {
      val xmlFile = XML.load("https://ru.wiktionary.org/wiki/" + word)

      //todo: add common table match for noun and adjectives
      val nounNode = (xmlFile \\ "a")
        .filter(href => href.attribute("title").toString.contains("существительное"))
      if (nounNode.length != 0) {
        val casesTable =
          for {
            table <- xmlFile \\ "tbody"
            thNodes = table \\ "tr" \\ "th"
            if isCasesTable(thNodes)
          } yield table

        val trNodes = casesTable.head.child.filter(_.child.nonEmpty)
        val cases = scala.collection.mutable.Map[String, (String, String)]()

        for (rowNum <- 1 until trNodes.size) {
          val row = trNodes(rowNum) \\ "td"
          //todo: switch to class representation
          cases += (row(0) \\ "a") (0).attribute("title").toString -> (row(1).text, row(2).text)
        }
      }
    }
    catch {
      case ex: Exception => {
        //todo: handle exceptions
      }
    }

  }

  private def isCasesTable(thNodes: NodeSeq): Boolean = {
    if (thNodes.length == 3 && (thNodes(0) \\ "a").text.matches(".*падеж.*")
      && (thNodes(1) \\ "a").text.matches(".*ед\\..*\\ч.*")
      && (thNodes(2) \\ "a").text.matches(".*мн\\..*ч\\..*")) {
      true
    }
    else false
  }
}
