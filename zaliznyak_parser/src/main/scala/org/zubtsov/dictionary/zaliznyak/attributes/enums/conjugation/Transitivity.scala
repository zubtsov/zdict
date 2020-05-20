package org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation

object Transitivity extends Enumeration {
  type Transitivity = Value
  val Transitive, Intransitive, Ditransitive = Value

  def apply(reflexive: Boolean, transitive: Option[String]) = {
    if (!reflexive && transitive.isEmpty)
      Transitive
    else if (reflexive)
      Intransitive
    else
      Ditransitive
  }
}
