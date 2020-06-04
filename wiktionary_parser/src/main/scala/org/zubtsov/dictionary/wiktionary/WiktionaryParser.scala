package org.zubtsov.dictionary.wiktionary

import java.io.FileWriter

import com.google.gson.Gson
import org.zubtsov.dictionary.config.{AdjectiveConfig, Config, NounConfig}
import org.zubtsov.dictionary.extractor.URLExtractor
import org.zubtsov.dictionary.html.HTMLDocument

import scala.io.Source
import scala.util.Try

//todo: add multithreading
object WiktionaryParser {

  def parse(filepath: String, outputPath: String): Unit = {
    val readLines = Source.fromFile(filepath, "Windows-1251").getLines()
      .filter(_.trim.nonEmpty)
      .map(line => line.split("\\s+")(0))
      .toList
      .distinct

    val parsedLines = readLines
      .map(line => Try({
        extractData(line, outputPath)
        line
      }))

    val parsedPages = parsedLines
      .filter(_.isSuccess)
      .map(_.get)

    val errors = parsedLines
      .filter(_.isFailure)
  }

  def extractData(word: String, outputPath: String): Unit = {

    //todo: handel connection exceptions
    val htmlDocument = URLExtractor.extractDataFromHTML("https://ru.wiktionary.org/wiki/" + word)

    htmlDocument.getElementsFromArticleByTag("a")
      .map(anchor => anchor.attribute("title").getOrElse(None).toString)
      .filter(anchor => anchor.contains("существительное") || anchor.contains("прилагательное"))
      .map(anchor => {
        if (anchor.contains("существительное")) NounConfig
        else AdjectiveConfig
      })
      .distinct
      .foreach(config => parseConfig(config, htmlDocument, word, outputPath))

  }

  private def parseConfig(config: Config, htmlDocument: HTMLDocument, word: String, outputPath: String): Unit = {
    val lexemeList = config.getParser.parse(htmlDocument, word)
    val gson = new Gson

    for (lexeme <- lexemeList.indices) {
      val writer = new FileWriter(outputPath + word + "_" + lexeme + ".json")
      writer.write(gson.toJson(lexemeList(lexeme)))
      writer.close()
    }
    lexemeList.foreach(println(_))

  }
}