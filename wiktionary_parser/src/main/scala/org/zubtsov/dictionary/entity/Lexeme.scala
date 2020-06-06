package org.zubtsov.dictionary.entity

import scala.collection.mutable.ListBuffer

case class Lexeme(initialForm: String, partOfSpeech: String, forms: ListBuffer[LexemeForm]){
  override def toString: String = initialForm + " " + partOfSpeech + " " + forms.mkString("|")
}