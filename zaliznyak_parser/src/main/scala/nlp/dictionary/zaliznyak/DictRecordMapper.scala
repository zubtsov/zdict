package nlp.dictionary.zaliznyak

import nlp.dictionary.zaliznyak.entity.VerbDictionaryRecord
import nlp.dictionary.zaliznyak.partofspeech.{Adjective, Noun, Verb}

import scala.util.Try

object DictRecordMapper {
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

  def map(dictrecord: VerbDictionaryRecord) = {
    Verb(dictrecord)
  }
}
