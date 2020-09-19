package nlp.dictionary.zaliznyak

import grizzled.slf4j.Logger

import scala.io.Source
import scala.util.Try

//todo: rename to ZaliznyakDictionary?
class ZaliznyakParser(filepath: String) {
  private val logger = Logger[this.type]
  private var successes: Seq[String] = _
  private var failures: Seq[Throwable] = _

  def parse() = {
    //      val readLines = List("весь 2 мс-п 2*в @ _мн._ все, всех, всем; _Т. ед. м, с_ всем ")
    //      val readLines = List("вкрасться 4 св 7в (-д-) $I")
    //      val readLines = List("попечь 4 св 8в/в (-к-)")
    val readLines = Source.fromFile(filepath, "Windows-1251").getLines()
      .filter(_.trim.nonEmpty).toList

    val parsedLines = readLines
      .map(line => {
        logger.debug(s"Parsing $line")
        Try(DictionaryRecordToPartOfSpeechMapping.map(line).toString)
      })

    successes = parsedLines
      .filter(_.isSuccess)
      .map(_.get)

    failures = parsedLines
      .filter(_.isFailure)
      .map(_.failed.get)

    this
  }

  def printSuccesses() = {
    println(successes.mkString("\n"))
    println(s"Number of parsed records: ${successes.size}")
    this
  }

  def printFailures() = {
    println(failures.mkString("\n"))
    println(s"Number of not-parsed records: ${failures.size}")
    this
  }
}