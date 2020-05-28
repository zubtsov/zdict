package org.zubtsov.dictionary.entity

case class Lexeme(initialForm: String, partOfSpeech: String, forms: Array[LexemeForm]){
  override def toString: String = initialForm + " " + partOfSpeech + " " + forms.mkString("|")
}