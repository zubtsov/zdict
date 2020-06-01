package nlp.dictionary.zaliznyak.conjugation

import nlp.dictionary.zaliznyak.feature.conjugation.{HasAspect, VerbPrimaryConjugationType, VerbWithStem, VerbWithStress}
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Aspect, Person, PrimaryConjugationType, Tense}
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}

trait ConjugatedForm extends VerbPrimaryConjugationType with VerbWithStress with HasAspect with VerbWithStem {
  private def stressDependantEnding(stressed: String, notStressed: String) = {
    if (isEndingStressed()) stressed else notStressed
  }

  def verbForm() = {
    val stem = (tense, aspect) match {
      case (Tense.Present, Aspect.Perfect) | (Tense.Future, Aspect.Imperfect) => {
        stemOfPresentFutureTense()
      }
      case (Tense.Past, _) => ???
    }

    val ending = (tense, aspect) match {
      case (Tense.Present, Aspect.Perfect) | (Tense.Future, Aspect.Imperfect) => {
        endingOfPresentFutureTense()
      }
      case (Tense.Past, _) => {
        number match {
          case Number.Singular => gender match {
            case Gender.Masculine => "л" //todo: после согласной - нуль
            case Gender.Feminine => "ла"
            case Gender.Neuter => "ло"
            case _ => ???
          }
          case Number.Plural => "ли"
          case _ => ???
        }
      }
      case _ => ???
    }

    (stem,ending)
  }

  private def endingOfPresentFutureTense() = {
    primaryConjugationType() match {
      case PrimaryConjugationType.First => {
        number match {
          case Number.Singular => person match {
            case Person.First => stressDependantEnding("ю", "у")
            case Person.Second => stressDependantEnding("ёшь", "ешь")
            case Person.Third => stressDependantEnding("ёт", "ет")
            case _ => ???
          }
          case Number.Plural => person match {
            case Person.First => stressDependantEnding("ём", "ем")
            case Person.Second => stressDependantEnding("ёте", "ете")
            case Person.Third => stressDependantEnding("ют", "ут")
            case _ => ???
          }
          case _ => ???
        }
      }
      case PrimaryConjugationType.Second => {
        number match {
          case Number.Singular => person match {
            case Person.First => "ю" //todo: после шипящих - у
            case Person.Second => "ишь"
            case Person.Third => "ит"
            case _ => ???
          }
          case Number.Plural => person match {
            case Person.First => "им"
            case Person.Second => "ите"
            case Person.Third => "ят" //todo: после шипящих - ат
            case _ => ???
          }
          case _ => ???
        }
      }
      case _ => ???
    }
  }

  private def stemOfPresentFutureTense() = {
    primaryConjugationType match {
      case PrimaryConjugationType.First => (person, number) match {
        case (Person.First, Number.Singular) | (Person.Third, Number.Plural) => stemOfFirstPersonSingularPresentTense()
        case _ => stemOfThirdPersonSingularPresentTense()
      }
      case PrimaryConjugationType.Second =>
        (person, number) match {
          case (Person.First, Number.Singular) => stemOfFirstPersonSingularPresentTense()
          case _ => stemOfThirdPersonSingularPresentTense()
        }
      case _ => ???
    }
  }
}
