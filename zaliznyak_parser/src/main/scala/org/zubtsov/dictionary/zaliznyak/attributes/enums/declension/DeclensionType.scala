package org.zubtsov.dictionary.zaliznyak.attributes.enums.declension

//aka Тип склонения
object DeclensionType extends Enumeration {
  type DeclensionType = Value
  //aka Субстантивное, Адъективное, Местоименное
  val Substantive, Adjectival, Pronounative = Value //todo: think about a better translation
  //aka Модификатор склонения
  def apply(declensionIndicator: String) = declensionIndicator match {
    case "м" | "мо" | "ж" | "жо" | "с" | "со" | "мо-жо" => DeclensionType.Substantive
    case "п" => DeclensionType.Adjectival
    case "мс" | "мс-п" => DeclensionType.Pronounative
    case _ => ???
  }
}
