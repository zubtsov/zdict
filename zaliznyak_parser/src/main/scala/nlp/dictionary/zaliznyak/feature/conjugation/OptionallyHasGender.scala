package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender

trait OptionallyHasGender {
  def gender(): Option[Gender]
}
