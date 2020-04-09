package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.attributes.enums.PrimaryStressType.PrimaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.SecondaryStressType.SecondaryStressType

trait HasStressType {
  def primaryStressType: PrimaryStressType
  def secondaryStressType: Option[SecondaryStressType]
}
