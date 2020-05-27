package nlp.dictionary.zaliznyak.feature.common

trait HasStem {
  def stem: String
  def hasVolatileVowel: Boolean
}
