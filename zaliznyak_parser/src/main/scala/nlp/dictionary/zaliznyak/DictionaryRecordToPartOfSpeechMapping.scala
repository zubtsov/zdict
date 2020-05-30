package nlp.dictionary.zaliznyak

import nlp.dictionary.zaliznyak.partofspeech.{Adjective, Noun, Verb}

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
      else {
        val verb = Try(Verb(dictrecord))
        if (verb.isSuccess)
          verb.get
        else ???
      }
    }
  }
}
