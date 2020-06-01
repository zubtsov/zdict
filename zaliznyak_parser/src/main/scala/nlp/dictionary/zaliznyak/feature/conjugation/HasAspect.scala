package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.enums.conjugation.Aspect.Aspect

trait HasAspect {
  def aspect(): Aspect
}
