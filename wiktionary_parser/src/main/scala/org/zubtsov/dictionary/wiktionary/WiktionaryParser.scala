package org.zubtsov.dictionary.wiktionary

import java.io.FileWriter

import com.google.gson.Gson
import org.zubtsov.dictionary.config.{AdjectiveConfig, Config, NounConfig}
import org.zubtsov.dictionary.extractor.URLExtractor

import scala.io.Source
import scala.xml.Elem

//todo: add multithreading
object WiktionaryParser {

  def parse(filepath: String, outputPath: String): Unit = {
    Source.fromFile(filepath, "Windows-1251").getLines()
      .filter(_.trim.nonEmpty)
      .map(line => line.split("\\s+")(0))
      .toList
      .distinct
      .foreach(extractData(_, outputPath))
  }

  def extractData(word: String, outputPath: String): Unit = {
    try {
      val xmlFile = URLExtractor.extractDataFromHTML("https://ru.wiktionary.org/wiki/" + word)

      (xmlFile \\ "a").foreach(h => {
        h.attribute("title").getOrElse("").toString match {
          case "существительное" => parseConfig(NounConfig, xmlFile, word, outputPath)
          case "прилагательное" => parseConfig(AdjectiveConfig, xmlFile, word, outputPath)
          case _ => "not noun or adj."
        }
      })
    }
    catch {
      case ex: Exception => {
        //todo: handle exceptions
      }
    }
  }

  private def parseConfig(config: Config, xmlFile: Elem, word: String, outputPath: String) {
    val lexemeList = config.getParser.parse(xmlFile, word)
    val gson = new Gson

    for (lexeme <- lexemeList.indices) {
      val writer = new FileWriter(outputPath + word + "_" + lexeme + ".json")
      writer.write(gson.toJson(lexemeList(lexeme)))
      writer.close()
    }
  }
}