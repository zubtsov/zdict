package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.enums.conjugation.Person.Person

trait OptionallyHasPerson {
  def person(): Option[Person]
}
