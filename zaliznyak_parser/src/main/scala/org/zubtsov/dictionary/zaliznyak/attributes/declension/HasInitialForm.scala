package org.zubtsov.dictionary.zaliznyak.attributes.declension

@deprecated("try to remove it. it's only workaround for volatile vowel conditions")
//todo: the situation has changed. is this also for infinitive?
trait HasInitialForm {
  def initialForm: String
}
