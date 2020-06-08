package org.zubtsov.dictionary.feature

object TenseEnum extends Enumeration {
  val Past, Present, Future = Value

  class Tense(tenseValue: String) extends Val(tenseValue)

  object Tense {

    def apply(tenseValue: String) = tenseValue match {
      case "наст." => new Tense("Present")
      case "прош." => new Tense("Past")
      case "будущ." => new Tense("Future")
      case _ => new Tense(tenseValue)
    }
  }
}