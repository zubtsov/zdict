package nlp.dictionary.zaliznyak

import nlp.dictionary.zaliznyak.partofspeech.{Adjective, Noun, PartOfSpeech, Verb}

object DictionaryRecordToPartOfSpeechMapping {
  def map(dictrecord: String) = {

    DictionaryRecord(dictrecord).getPartOfSpeech() match {
      case PartOfSpeech.Noun => Noun(dictrecord)
      case PartOfSpeech.Adjective => Adjective(dictrecord)
      case PartOfSpeech.Verb => Verb(dictrecord)
      case _ => throw new Exception(s"Unknown part of speech: $dictrecord")
    }
  }
}
