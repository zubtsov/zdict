package org.zubtsov.dictionary.feature

object CaseEnum extends Enumeration {

  class Case(caseValue: String) extends Val(caseValue)

  object Case {

    def apply(caseValue: String) = caseValue match {
      case "именительныи" => new Case("Nominative")
      case "родительныи" => new Case("Genetive")
      case "дательныи" => new Case("Dative")
      case "винительныи" => new Case("Accusative")
      case "творительныи" => new Case("Instrumental")
      case "предложныи" => new Case("Prepositional")
      case _ => new Case(caseValue)
    }
  }
}