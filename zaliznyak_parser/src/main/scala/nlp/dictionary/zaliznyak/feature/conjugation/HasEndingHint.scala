package nlp.dictionary.zaliznyak.feature.conjugation

trait HasEndingHint {
  def endingHint(): Option[String]
}
