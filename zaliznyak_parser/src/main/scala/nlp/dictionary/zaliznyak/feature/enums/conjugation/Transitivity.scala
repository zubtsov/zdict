package nlp.dictionary.zaliznyak.feature.enums.conjugation
//aka Переходность
object Transitivity extends Enumeration {
  type Transitivity = Value
  //aka Переходный, Непереходный, Двувидовой
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
