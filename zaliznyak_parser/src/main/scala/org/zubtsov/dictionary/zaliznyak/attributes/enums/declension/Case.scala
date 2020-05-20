package org.zubtsov.dictionary.zaliznyak.attributes.enums.declension

//aka Падеж
object Case extends Enumeration {
  type Case = Value
  //aka Именительный, Родительный, Дательный, Винительный, Творительный, Предложный
  val Nominative, Genetive, Dative, Accusative, Instrumental, Prepositional = Value
}
