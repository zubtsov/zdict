package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.PrimaryStressType.PrimaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.SecondaryStressType.SecondaryStressType

trait HasStressType {
  def primaryStressType: PrimaryStressType
  def secondaryStressType: Option[SecondaryStressType]
}
