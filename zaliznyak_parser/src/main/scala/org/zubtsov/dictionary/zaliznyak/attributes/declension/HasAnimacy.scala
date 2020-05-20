package org.zubtsov.dictionary.zaliznyak.attributes.declension

import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.Animacy.Animacy

trait HasAnimacy {
  def animacy: Animacy
}
