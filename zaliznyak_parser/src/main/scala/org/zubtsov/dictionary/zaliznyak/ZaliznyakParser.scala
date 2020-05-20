package org.zubtsov.dictionary.zaliznyak

import org.zubtsov.dictionary.zaliznyak.entities.NameDictionaryRecord

import scala.io.Source
import scala.util.Try

class ZaliznyakParser(filepath: String) {
  //  val readLines = List("весь 2 мс-п 2*в @ _мн._ все, всех, всем; _Т. ед. м, с_ всем ")
  val readLines = Source.fromFile(filepath, "Windows-1251").getLines()
    .filter(_.trim.nonEmpty).toList

  val parsedLines = readLines
    .map(line => {
      Try(
        {
          val dictRecord = NameDictionaryRecord(line)
          val parsedWord = DictRecordMapper.map(dictRecord)
          parsedWord.toString
        }
      )
    })

  val matchedLines = parsedLines
    .filter(_.isSuccess)
    .map(_.get)

  val numberOfErrors = parsedLines
    .filter(_.isFailure).length
}