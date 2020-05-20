package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Gender.Gender

trait HasGender {
  def gender: Gender
}