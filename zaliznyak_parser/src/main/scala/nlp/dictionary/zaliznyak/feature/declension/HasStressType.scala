package nlp.dictionary.zaliznyak.feature.declension

import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType

trait HasStressType {
  def primaryStressType: PrimaryStressType
  def secondaryStressType: Option[SecondaryStressType]
}
