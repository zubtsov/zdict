package org.zubtsov.dictionary.zaliznyak.attributes.enums.declension

//aka Одушевленность
object Animacy extends Enumeration {
  type Animacy = Value
  val Animate, Inanimate = Value
  //aka Модификатор склонения
  def apply(declensionIdentifier: String): Option[Animacy] = declensionIdentifier match {
    case "м" | "ж" | "с" => Option(Inanimate)
    case "мо" | "жо" | "со" | "мо-жо" => Option(Animate)
    case "мс" | "мс-п" | "п" => None
    case _ => ???
  }
}
