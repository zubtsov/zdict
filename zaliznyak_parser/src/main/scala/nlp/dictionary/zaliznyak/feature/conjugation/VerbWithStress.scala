package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.feature.common.{HasNumber}
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Mood, PastStressType, Person, PresentStressType, Tense}

trait VerbWithStress extends HasPastAndPresentStressType with HasTense with HasNumber with OptionallyHasPerson with HasMood with OptionallyHasGender {
  def isStemStressed(): Boolean = {
    tense match {
      case Tense.Present | Tense.Future => presentStressType match {
        case PresentStressType.TypeA => true
        case PresentStressType.TypeB => false
        case PresentStressType.TypeC => {
          if (mood() == Mood.Imperative ||
            (person == Person.First && number == Number.Singular))
            false
          else
            true
        }
        case _ => ???
      }
      case Tense.Past => pastStressType match {
        case PastStressType.TypeA => true
        case PastStressType.TypeB => if (gender.get == Gender.Masculine) true else false
        case PastStressType.TypeC => false
        case PastStressType.TypeExtraC => ??? //todo: implement
        case _ => ???
      }
      case _ => ???
    }
  }

  def isEndingStressed(): Boolean = !isStemStressed()
}
