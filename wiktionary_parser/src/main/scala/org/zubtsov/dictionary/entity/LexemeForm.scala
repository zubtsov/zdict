package org.zubtsov.dictionary.entity

case class LexemeForm(form: String, grammaticalFeatures: Array[String]){
  override def toString: String = form + " " +  grammaticalFeatures.mkString(" ")
}