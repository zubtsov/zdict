package org.zubtsov.dictionary.zaliznyak.attributes

@deprecated("try to remove it. it's only workaround for volatile vowel conditions")
trait HasInitialForm {
  def initialForm: String
}
