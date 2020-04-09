package org.zubtsov.dictionary.zaliznyak.attributes.enums
//aka Второстепеная схема ударения
object SecondaryStressType extends Enumeration {
  type SecondaryStressType = Value
  val Apostrophe, DoubleApostrophe = Value

  def apply(secondaryStressTypes: String): Option[SecondaryStressType] = secondaryStressTypes match {
    case "'" => Option(Apostrophe)
    case "''" => Option(DoubleApostrophe)
    case _ => None
  }
}
