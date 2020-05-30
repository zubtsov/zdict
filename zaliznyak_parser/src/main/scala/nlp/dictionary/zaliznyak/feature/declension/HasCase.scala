package nlp.dictionary.zaliznyak.feature.declension

import nlp.dictionary.zaliznyak.feature.enums.declension.Case.Case

//rCase stands for "Russian case", because the word "case" conflicts with the corresponding Scala keyword
trait HasCase {
  def rCase: Case
}
