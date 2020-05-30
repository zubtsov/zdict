package nlp.dictionary.zaliznyak

import nlp.dictionary.zaliznyak.partofspeech.{Adjective, Noun}

import scala.util.Try
//todo: refactor
class DictionaryRecordToPartOfSpeechMapping {
  def map(dictrecord: String) = {
    val noun = Try(Noun(dictrecord))
    if (noun.isSuccess)
      noun.get
    else {
      val adjective = Try(Adjective(dictrecord))
      if (adjective.isSuccess)
        adjective.get
      else
        ???
    }
  }
}
