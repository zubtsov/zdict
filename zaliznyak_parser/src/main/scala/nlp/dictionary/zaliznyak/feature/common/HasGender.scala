package nlp.dictionary.zaliznyak.feature.common

import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender

trait HasGender {
  def gender: Gender
}
