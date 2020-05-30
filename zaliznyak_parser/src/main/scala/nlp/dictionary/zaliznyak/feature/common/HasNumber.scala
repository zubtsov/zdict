package nlp.dictionary.zaliznyak.feature.common

import nlp.dictionary.zaliznyak.feature.enums.common.Number.Number

//doesn't necessarily mean countable
trait HasNumber {
  def number: Number
}
