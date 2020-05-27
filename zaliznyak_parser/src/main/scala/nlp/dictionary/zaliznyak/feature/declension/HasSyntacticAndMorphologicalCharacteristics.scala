package nlp.dictionary.zaliznyak.feature.declension

@deprecated("try to remove it or remove at least morphological characteristic")
trait HasSyntacticAndMorphologicalCharacteristics {
  def primarySyntacticCharacteristic: String

  def primaryMorphologicalCharacteristic: String
}
