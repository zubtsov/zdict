package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.enums.conjugation.PastStressType.PastStressType
import nlp.dictionary.zaliznyak.feature.enums.conjugation.PresentStressType.PresentStressType

trait HasPastAndPresentStressType {
  def pastStressType: PastStressType
  def presentStressType: PresentStressType
}
