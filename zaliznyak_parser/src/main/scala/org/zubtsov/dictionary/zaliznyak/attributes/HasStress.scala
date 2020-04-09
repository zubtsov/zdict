package org.zubtsov.dictionary.zaliznyak.attributes

trait HasStress {
  private var _isEndingStressed: Boolean = _

  def isStemStressed: Boolean = !isEndingStressed

  def isEndingStressed: Boolean = _isEndingStressed

  def isEndingStressed_=(stressed: Boolean): HasStress = {
    _isEndingStressed = stressed
    this
  }
}
