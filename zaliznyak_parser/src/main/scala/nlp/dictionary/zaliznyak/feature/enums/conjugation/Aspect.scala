package nlp.dictionary.zaliznyak.feature.enums.conjugation

//aka Вид
object Aspect extends Enumeration {
  type Aspect = Value
  //aka Совершенный, Несовершенный, Двувидовой
  val Perfect, Imperfect, Mixed = Value

  def apply(aspect: String) = {
    aspect match {
      case "св" => Perfect
      case "нсв" => Imperfect
      case "св-нсв" => Mixed
      case _ => ???
    }
  }
}
