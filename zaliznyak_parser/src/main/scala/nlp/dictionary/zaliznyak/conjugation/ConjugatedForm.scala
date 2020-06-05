package nlp.dictionary.zaliznyak.conjugation

import nlp.dictionary.zaliznyak.feature.conjugation.{HasAspect, VerbPrimaryConjugationType, VerbWithStem, VerbWithStress}
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Aspect, Person, PrimaryConjugationType, Tense}
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}

trait ConjugatedForm extends VerbPrimaryConjugationType with VerbWithStress with HasAspect with VerbWithStem {
  private def stressDependantEnding(stressed: String, notStressed: String) = {
    if (isEndingStressed()) stressed else notStressed
  }

  def verbForm() = {
    if (
      ((aspect == Aspect.Perfect && tense == Tense.Future) || (aspect == Aspect.Imperfect && tense == Tense.Present))
        && number == Number.Singular
        && (person == Some(Person.First) || person == Some(Person.Third))
    ) {
      formOfFirstOrThirdPersonPresentSingular()
    }
    else {
      val stem = (tense, aspect) match {
        case (Tense.Present, Aspect.Imperfect) | (Tense.Future, Aspect.Perfect) => {
          stemOfPresentFutureTense()
        }
        case (Tense.Past, _) => {
          conjugationType match {
            case 7 | 8 => ???
            case _ => stemOfInfinitive()
          }
        }
      }

      val ending = (tense, aspect) match {
        case (Tense.Present, Aspect.Imperfect) | (Tense.Future, Aspect.Perfect) => {
          endingOfPresentFutureTense(stem.takeRight(1))
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

      (stem, ending)
    }
  }

  private def endingOfPresentFutureTense(lastLetterOfStem: String) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    primaryConjugationType() match {
      case PrimaryConjugationType.First => {
        number match {
          case Number.Singular => person match {
            case Some(Person.First) => if (lastLetterOfStem.isVowel() || lastLetterOfStem == "ь" || lastLetterOfStem == "л") "ю" else "у"
            case Some(Person.Second) => stressDependantEnding("ёшь", "ешь")
            case Some(Person.Third) => stressDependantEnding("ёт", "ет")
            case _ => ???
          }
          case Number.Plural => person match {
            case Some(Person.First) => stressDependantEnding("ём", "ем")
            case Some(Person.Second) => stressDependantEnding("ёте", "ете")
            case Some(Person.Third) => if (lastLetterOfStem.isVowel() || lastLetterOfStem == "ь" || lastLetterOfStem == "л") "ют" else "ут"
            case _ => ???
          }
          case _ => ???
        }
      }
      case PrimaryConjugationType.Second => {
        number match {
          case Number.Singular => person match {
            case Some(Person.First) => "ю" //todo: после шипящих - у
            case Some(Person.Second) => "ишь"
            case Some(Person.Third) => "ит"
            case _ => ???
          }
          case Number.Plural => person match {
            case Some(Person.First) => "им"
            case Some(Person.Second) => "ите"
            case Some(Person.Third) => "ят" //todo: после шипящих - ат
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
        case (Some(Person.First), Number.Singular) | (Some(Person.Third), Number.Plural) => stemOfFirstPersonSingularPresentTense()
        case _ => stemOfThirdPersonSingularPresentTense()
      }
      case PrimaryConjugationType.Second =>
        (person, number) match {
          case (Some(Person.First), Number.Singular) => stemOfFirstPersonSingularPresentTense()
          case _ => stemOfThirdPersonSingularPresentTense()
        }
      case _ => ???
    }
  }
}
