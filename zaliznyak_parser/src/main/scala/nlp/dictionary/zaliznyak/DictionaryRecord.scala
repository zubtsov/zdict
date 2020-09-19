package nlp.dictionary.zaliznyak

import nlp.dictionary.zaliznyak.partofspeech.{Adjective, Noun, PartOfSpeech, Verb}
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

case class DictionaryRecord(record: String) {
  def getPartOfSpeech(): PartOfSpeech = {
    record match {
      case Noun.regex(_*) => PartOfSpeech.Noun
      case Adjective.regex(_*) => PartOfSpeech.Adjective
      case Verb.regex(_*) => PartOfSpeech.Verb
    }
  }
}
