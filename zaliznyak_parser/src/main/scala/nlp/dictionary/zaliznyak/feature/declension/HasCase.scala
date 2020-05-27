package nlp.dictionary.zaliznyak.feature.declension

import nlp.dictionary.zaliznyak.feature.enums.declension.Case.Case

//rCase stands for "Russian case", because the word "case" conflicts with the corresponding Scala keyword
trait HasCase {
  private var _case: Case = _

  def rCase_=(rCase: Case): HasCase = {
    _case = rCase
    this
  }

  def rCase: Case = _case
}
