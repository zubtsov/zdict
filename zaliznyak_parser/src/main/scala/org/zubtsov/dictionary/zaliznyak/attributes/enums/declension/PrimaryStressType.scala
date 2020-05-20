package org.zubtsov.dictionary.zaliznyak.attributes.enums.declension

//aka Схема ударения
object PrimaryStressType extends Enumeration {
  type PrimaryStressType = Value
  val a,b,c,d,e,f = Value

  def apply(primaryStressType: String): PrimaryStressType = primaryStressType match {
    case "а" => PrimaryStressType.a
    case "в" => PrimaryStressType.b
    case "с" => PrimaryStressType.c
    case "D" => PrimaryStressType.d
    case "е" => PrimaryStressType.e
    case "F" => PrimaryStressType.f
    case _ => ???
  }
}
