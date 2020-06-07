package nlp.dictionary.zaliznyak.feature.declension

import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender

trait HasGender {
  def gender: Gender
}
