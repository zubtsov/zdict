package org.zubtsov.dictionary.zaliznyak.attributes.enums
//aka Род
object Gender extends Enumeration {
  type Gender = Value
  //aka Мужской, Женский, Средний, Общий
  val Masculine, Feminine, Neuter, Common = Value

  def commonValues = values - Common
  //aka Основная синтаксическая характеристика
  def apply(primarySyntacticCharacteristic: String) = primarySyntacticCharacteristic match {
    case "м" | "мо" => Masculine
    case "ж" | "жо" => Feminine
    case "с" | "со" => Neuter
    case "мо-жо" => Common
    case "п" => Masculine
    case "мс" => Masculine
    case "мс-п" => Masculine
    case _ => ???
  }
}
