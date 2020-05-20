package org.zubtsov.dictionary.zaliznyak.declension.types

import org.zubtsov.dictionary.zaliznyak.attributes.{enums, _}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Gender
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.{Animacy, Case}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Animacy, Number}

//todo: how to treat Common gender?
//todo: add recursive calls?

//aka Субстантивное склонение
object SubstantiveDeclension {
  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    declensionSubtype match {
      case 1 => endingOfSubtype1(declensionParameters)
      case 2 => endingOfSubtype2(declensionParameters)
      case _ => ???
    }
  }

  def endingOfSubtype1(declensionParameters: HasGender with HasNumber with HasCase with HasAnimacy) = {
    import declensionParameters._
    number match {
      case enums.common.Number.Singular => gender match {
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
      case enums.common.Number.Plural => gender match {
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

  def endingOfSubtype2(declensionParameters: HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._
    number match {
      case enums.common.Number.Singular => gender match {
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
          case Case.Prepositional => endingOfSubtype1(declensionParameters)
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "я"
          case Case.Genetive => "и"
          case Case.Dative | Case.Prepositional => endingOfSubtype1(declensionParameters)
          case Case.Accusative => "ю"
          case Case.Instrumental => if (isEndingStressed) "ёй" else "ей" //todo: добавь "ёю", "ею"?
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative | Case.Accusative => if (isEndingStressed) "ё" else "е"
          case Case.Genetive => "я"
          case Case.Dative => "ю"
          case Case.Instrumental => if (isEndingStressed) "ём" else "ем"
          case Case.Prepositional => endingOfSubtype1(declensionParameters)
          case _ => ???
        }
      }
      case enums.common.Number.Plural => gender match {
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
