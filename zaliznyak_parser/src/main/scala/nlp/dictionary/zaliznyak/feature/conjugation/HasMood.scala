package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.enums.conjugation.Mood.Mood

trait HasMood {
  def mood(): Mood
}
