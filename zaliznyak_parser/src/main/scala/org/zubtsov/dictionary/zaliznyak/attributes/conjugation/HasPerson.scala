package org.zubtsov.dictionary.zaliznyak.attributes.conjugation

import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.Person.Person

trait HasPerson {
  def person: Person
}
