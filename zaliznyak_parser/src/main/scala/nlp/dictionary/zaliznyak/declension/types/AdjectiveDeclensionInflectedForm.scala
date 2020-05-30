package nlp.dictionary.zaliznyak.declension.types

import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype}
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Animacy, Case}
import nlp.dictionary.zaliznyak.stress.WordWithStress

//todo: add recursive calls?
//aka Адъективное склонение
trait AdjectiveDeclensionInflectedForm extends HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with WordWithStress {
  protected def adjectiveEnding() = {
    declensionSubtype match {
      case 1 => adjectiveEndingOfSubtype1()
      case 2 => adjectiveEndingOfSubtype2()
      case _ => ???
    }
  }

  protected def shortFormAdjectiveEnding() = {
    declensionSubtype match {
      case 1 => shortFormAdjectiveEndingOfSubtype1()
      case 2 => shortFormAdjectiveEndingOfSubtype2()
      case _ => ???
    }
  }

  protected def adjectiveEndingOfSubtype1() = {
    number match {
      case Number.Singular => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => if (isEndingStressed) "ой" else "ый"
          case Case.Genetive => "ого"
          case Case.Dative => "ому"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "ого"
            case Animacy.Inanimate => if (isEndingStressed) "ой" else "ый"
            case _ => ???
          }
          case Case.Instrumental => "ым"
          case Case.Prepositional => "ом"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "ая"
          case Case.Genetive => "ой"
          case Case.Dative => "ой"
          case Case.Accusative => "ую"
          case Case.Instrumental => "ой" //todo: добавь "ою"?
          case Case.Prepositional => "ой"
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative => "ое"
          case Case.Genetive => "ого"
          case Case.Dative => "ому"
          case Case.Accusative => "ое"
          case Case.Instrumental => "ым"
          case Case.Prepositional => "ом"
          case _ => ???
        }
        case _ => ???
      }
      case common.Number.Plural => rCase match {
        case Case.Nominative => "ые"
        case Case.Genetive => "ых"
        case Case.Dative => "ым"
        case Case.Accusative => animacy match {
          case Animacy.Animate => "ых"
          case Animacy.Inanimate => "ые"
          case _ => ???
        }
        case Case.Instrumental => "ыми"
        case Case.Prepositional => "ых"
        case _ => ???
      }
      case _ => ???
    }
  }

  protected def adjectiveEndingOfSubtype2() = {
    number match {
      case common.Number.Singular => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => "ий"
          case Case.Genetive => "его"
          case Case.Dative => "ему"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "его"
            case Animacy.Inanimate => "ий"
            case _ => ???
          }
          case Case.Instrumental => "им"
          case Case.Prepositional => "ем"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "яя"
          case Case.Genetive => "ей"
          case Case.Dative => "ей"
          case Case.Accusative => "юю"
          case Case.Instrumental => "ей" //todo: добавь "ею"?
          case Case.Prepositional => "ей"
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative => "ее"
          case Case.Genetive => "его"
          case Case.Dative => "ему"
          case Case.Accusative => "ее"
          case Case.Instrumental => "им"
          case Case.Prepositional => "ем"
          case _ => ???
        }
        case _ => ???
      }
      case common.Number.Plural => rCase match {
        case Case.Nominative => "ие"
        case Case.Genetive => "их"
        case Case.Dative => "им"
        case Case.Accusative => animacy match {
          case Animacy.Animate => "их"
          case Animacy.Inanimate => "ие"
          case _ => ???
        }
        case Case.Instrumental => "ими"
        case Case.Prepositional => "их"
        case _ => ???
      }
      case _ => ???
    }
  }

  protected def shortFormAdjectiveEndingOfSubtype1() = {
    number match {
      case common.Number.Singular => gender match {
        case Gender.Masculine => ""
        case Gender.Feminine => "а"
        case Gender.Neuter => "о"
        case _ => ???
      }
      case common.Number.Plural => "ы"
      case _ => ???
    }
  }

  protected def shortFormAdjectiveEndingOfSubtype2() = {
    number match {
      case common.Number.Singular => gender match {
        case Gender.Masculine => "ь"
        case Gender.Feminine => "я"
        case Gender.Neuter => if (isEndingStressed) "ё" else "е"
        case _ => ???
      }
      case common.Number.Plural => "и"
      case _ => ???
    }
  }
}
