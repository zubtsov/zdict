package nlp.dictionary.zaliznyak.feature.declension

import nlp.dictionary.zaliznyak.feature.enums.declension.Animacy.Animacy

trait HasAnimacy {
  def animacy: Animacy
}
