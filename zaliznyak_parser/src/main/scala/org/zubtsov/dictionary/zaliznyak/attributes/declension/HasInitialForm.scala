package org.zubtsov.dictionary.zaliznyak.attributes.declension

@deprecated("try to remove it. it's only workaround for volatile vowel conditions")
trait HasInitialForm {
  def initialForm: String
}
