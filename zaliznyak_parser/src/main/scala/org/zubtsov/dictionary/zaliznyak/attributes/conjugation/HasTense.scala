package org.zubtsov.dictionary.zaliznyak.attributes.conjugation

import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.Tense.Tense

trait HasTense {
  def tense: Tense
}
