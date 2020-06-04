package nlp.dictionary.zaliznyak.feature.enums.conjugation

object PastStressType extends Enumeration {
  type PastStressType = Value
  val TypeA, TypeB, TypeC, TypeExtraC = Value

  def apply(st: String): PastStressType = {
    val stressType = if (st.contains("/")) {
      st.split("/")(1)
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
