package nlp.dictionary.zaliznyak.feature.enums.conjugation

object PresentStressType extends Enumeration {
  type PresentStressType = Value
  val TypeA, TypeB, TypeC = Value

  def apply(st: String): PresentStressType = {
    val stressType = if (st.contains("/")) {
      st.split("/")(0)
    } else {
      st
    }

    stressType match {
      case "а" => TypeA
      case "в" => TypeB
      case "с" => TypeC
      case _ => ???
    }
  }
}
