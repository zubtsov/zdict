package org.zubtsov.dictionary.zaliznyak.declension.types

import org.zubtsov.dictionary.zaliznyak.attributes.common.{HasGender, HasNumber}
import org.zubtsov.dictionary.zaliznyak.attributes.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasStress}
import org.zubtsov.dictionary.zaliznyak.attributes.{enums, _}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Gender
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.{Animacy, Case}

//todo: add recursive calls?
//aka Местоименное склонение
object PronounativeDeclension {
  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    declensionSubtype match {
      case 1 => endingOfSubtype1(declensionParameters)
      case 2 => endingOfSubtype2(declensionParameters)
      case _ => ???
    }
  }

  def endingOfSubtype1(declensionParameters: HasGender with HasNumber with HasCase with HasAnimacy ) = {
    import declensionParameters._
    number match {
      case enums.common.Number.Singular => gender match {
        case Gender.Masculine => rCase match {
          case Case.Nominative => ""
          case Case.Genetive => "ого"
          case Case.Dative => "ому"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "ого"
            case Animacy.Inanimate => ""
            case _ => ???
          }
          case Case.Instrumental => "ым"
          case Case.Prepositional => "ом"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "а"
          case Case.Genetive => "ой"
          case Case.Dative => "ой"
          case Case.Accusative => "у"
          case Case.Instrumental => "ой" //todo: добавь "ою"?
          case Case.Prepositional => "ой"
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative => "о"
          case Case.Genetive => "ого"
          case Case.Dative => "ому"
          case Case.Accusative => "о"
          case Case.Instrumental => "ым"
          case Case.Prepositional => "ом"
          case _ => ???
        }
        case _ => ???
      }
      case enums.common.Number.Plural => rCase match {
        case Case.Nominative => "ы"
        case Case.Genetive => "ых"
        case Case.Dative => "ым"
        case Case.Accusative => animacy match {
          case Animacy.Animate => "ых"
          case Animacy.Inanimate => "ы"
          case _ => ???
        }
        case Case.Instrumental => "ыми"
        case Case.Prepositional => "ых"
        case _ => ???
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
          case Case.Genetive => "его"
          case Case.Dative => "ему"
          case Case.Accusative => animacy match {
            case Animacy.Animate => "его"
            case Animacy.Inanimate => "ь"
            case _ => ???
          }
          case Case.Instrumental => "им"
          case Case.Prepositional => if (isEndingStressed) "ём" else "ем"
          case _ => ???
        }
        case Gender.Feminine => rCase match {
          case Case.Nominative => "я"
          case Case.Genetive => "ей"
          case Case.Dative => "ей"
          case Case.Accusative => "ю"
          case Case.Instrumental => "ей" //todo: добавь "ею"?
          case Case.Prepositional => "ей"
          case _ => ???
        }
        case Gender.Neuter => rCase match {
          case Case.Nominative | Case.Accusative => if (isEndingStressed) "ё" else "е"
          case Case.Genetive => "его"
          case Case.Dative => "ему"
          case Case.Instrumental => "им"
          case Case.Prepositional => if (isEndingStressed) "ём" else "ем"
          case _ => ???
        }
        case _ => ???
      }
      case enums.common.Number.Plural => rCase match {
        case Case.Nominative => "и"
        case Case.Genetive => "их"
        case Case.Dative => "им"
        case Case.Accusative => animacy match {
          case Animacy.Animate => "их"
          case Animacy.Inanimate => "и"
          case _ => ???
        }
        case Case.Instrumental => "ими"
        case Case.Prepositional => "их"
        case _ => ???
      }
      case _ => ???
    }
  }
}
