package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.attributes.enums.Number.Number
//doesn't necessarily mean countable
trait HasNumber {
  private var _number: Number = _

  def number_=(number: Number): HasNumber = {
    _number = number
    this
  }

  def number: Number = _number
}
