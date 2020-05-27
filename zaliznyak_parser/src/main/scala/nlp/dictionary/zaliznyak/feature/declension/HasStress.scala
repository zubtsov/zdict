package nlp.dictionary.zaliznyak.feature.declension

trait HasStress {
  private var _isEndingStressed: Boolean = _

  def isStemStressed: Boolean = !isEndingStressed

  def isEndingStressed: Boolean = _isEndingStressed

  def isEndingStressed_=(stressed: Boolean): HasStress = {
    _isEndingStressed = stressed
    this
  }
}
