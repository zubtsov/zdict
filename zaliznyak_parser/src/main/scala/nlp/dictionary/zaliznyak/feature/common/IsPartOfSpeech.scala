package nlp.dictionary.zaliznyak.feature.common

import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

trait IsPartOfSpeech {
  def partOfSpeech: PartOfSpeech
}
