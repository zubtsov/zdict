package org.zubtsov.dictionary.zaliznyak.attributes.common

trait HasStem {
  def stem: String
  def hasVolatileVowel: Boolean
}
