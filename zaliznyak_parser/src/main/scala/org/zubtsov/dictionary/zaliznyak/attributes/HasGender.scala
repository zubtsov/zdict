package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.attributes.enums.Gender.Gender

trait HasGender {
  def gender: Gender
}