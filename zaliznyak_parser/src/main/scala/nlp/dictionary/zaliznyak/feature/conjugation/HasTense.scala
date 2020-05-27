package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.enums.conjugation.Tense.Tense

trait HasTense {
  def tense: Tense
}
