package nlp.dictionary.zaliznyak.declension.types

import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasStress}
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Animacy, Case}

//todo: how to treat Common gender?
//todo: add recursive calls?

//aka Субстантивное склонение
trait SubstantiveDeclensionInflectedForm extends HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress {
  protected def substantiveEnding() = {
    declensionSubtype match {
      case 1 => substantiveEndingOfSubtype1()
      case 2 => substantiveEndingOfSubtype2()
      case _ => ???
    }
  }

  protected def substantiveEndingOfSubtype1() = {
    number match {
      case Number.Singular => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => ""
          case Case.Genetive => "а"
          case Case.Dative => "у"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "а"
            case Animacy.Inanimate => ""
            case _ => ???
          }
          case Case.Instrumental => "ом"
          case Case.Prepositional => "е"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "а"
          case Case.Genetive => "ы"
          case Case.Dative | Case.Prepositional => "е"
          case Case.Accusative => "у"
          case Case.Instrumental => "ой" //todo: добавь "ою"?
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative | Case.Accusative => "о"
          case Case.Genetive => "а"
          case Case.Dative => "у"
          case Case.Instrumental => "ом"
          case Case.Prepositional => "е"
          case _ => ???
        }
      }
      case common.Number.Plural => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => "ы"
          case Case.Genetive => "ов"
          case Case.Dative => "ам"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "ов"
            case Animacy.Inanimate => "ы"
            case _ => ???
          }
          case Case.Instrumental => "ами"
          case Case.Prepositional => "ах"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "ы"
          case Case.Genetive => ""
          case Case.Dative => "ам"
          case Case.Accusative => animacy match {
            case Animacy.Animate => ""
            case Animacy.Inanimate => "ы"
            case _ => ???
          }
          case Case.Instrumental => "ами"
          case Case.Prepositional => "ах"
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative => "а"
          case Case.Genetive => ""
          case Case.Dative => "ам"
          case Case.Accusative => animacy match {
            case Animacy.Animate => ""
            case Animacy.Inanimate => "а"
            case _ => ???
          }
          case Case.Instrumental => "ами"
          case Case.Prepositional => "ах"
          case _ => ???
        }
      }
      case _ => ???
    }
  }

  protected def substantiveEndingOfSubtype2() = {
    number match {
      case common.Number.Singular => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => "ь"
          case Case.Genetive => "я"
          case Case.Dative => "ю"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "я"
            case Animacy.Inanimate => "ь"
            case _ => ???
          }
          case Case.Instrumental => if (isEndingStressed) "ём" else "ем"
          case Case.Prepositional => substantiveEndingOfSubtype1()
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "я"
          case Case.Genetive => "и"
          case Case.Dative | Case.Prepositional => substantiveEndingOfSubtype1()
          case Case.Accusative => "ю"
          case Case.Instrumental => if (isEndingStressed) "ёй" else "ей" //todo: добавь "ёю", "ею"?
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative | Case.Accusative => if (isEndingStressed) "ё" else "е"
          case Case.Genetive => "я"
          case Case.Dative => "ю"
          case Case.Instrumental => if (isEndingStressed) "ём" else "ем"
          case Case.Prepositional => substantiveEndingOfSubtype1()
          case _ => ???
        }
      }
      case common.Number.Plural => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => "и"
          case Case.Genetive => "ей"
          case Case.Dative => "ям"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "ей"
            case Animacy.Inanimate => "и"
            case _ => ???
          }
          case Case.Instrumental => "ями"
          case Case.Prepositional => "ях"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "и"
          case Case.Genetive => if (isEndingStressed) "ей" else "ь"
          case Case.Dative => "ям"
          case Case.Accusative => animacy match {
            case Animacy.Animate => if (isEndingStressed) "ей" else "ь"
            case Animacy.Inanimate => "и"
            case _ => ???
          }
          case Case.Instrumental => "ями"
          case Case.Prepositional => "ях"
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative => "я"
          case Case.Genetive => if (isEndingStressed) "ей" else "ь"
          case Case.Dative => "ям"
          case Case.Accusative => animacy match {
            case Animacy.Animate => if (isEndingStressed) "ей" else "ь"
            case Animacy.Inanimate => "и"
            case _ => ???
          }
          case Case.Instrumental => "ями"
          case Case.Prepositional => "ях"
          case _ => ???
        }
      }
      case _ => ???
    }
  }
}
