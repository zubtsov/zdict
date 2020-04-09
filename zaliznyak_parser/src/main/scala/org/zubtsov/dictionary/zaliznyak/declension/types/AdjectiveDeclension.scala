package org.zubtsov.dictionary.zaliznyak.declension.types

import org.zubtsov.dictionary.zaliznyak.attributes._
import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Animacy, Case, Gender, Number}

//todo: add recursive calls?
//aka Адъективное склонение
object AdjectiveDeclension {
  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    declensionSubtype match {
      case 1 => endingOfSubtype1(declensionParameters)
      case 2 => endingOfSubtype2(declensionParameters)
      case _ => ???
    }
  }

  def shortFormEnding(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    declensionSubtype match {
      case 1 => shortFormEndingOfSubtype1(declensionParameters)
      case 2 => shortFormEndingOfSubtype2(declensionParameters)
      case _ => ???
    }
  }

  def endingOfSubtype1(declensionParameters: HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._
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
      case Number.Plural => rCase match {
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

  def endingOfSubtype2(declensionParameters: HasGender with HasNumber with HasCase with HasAnimacy) = {
    import declensionParameters._
    number match {
      case Number.Singular => gender match {
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
      case Number.Plural => rCase match {
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

  def shortFormEndingOfSubtype1(declensionParameters: HasGender with HasNumber) = {
    import declensionParameters._
    number match {
      case Number.Singular => gender match {
        case Gender.Masculine => ""
        case Gender.Feminine => "а"
        case Gender.Neuter => "о"
        case _ => ???
      }
      case Number.Plural => "ы"
      case _ => ???
    }
  }

  def shortFormEndingOfSubtype2(declensionParameters: HasGender with HasNumber with HasStress) = {
    import declensionParameters._
    number match {
      case Number.Singular => gender match {
        case Gender.Masculine => "ь"
        case Gender.Feminine => "я"
        case Gender.Neuter => if (isEndingStressed) "ё" else "е"
        case _ => ???
      }
      case Number.Plural => "и"
      case _ => ???
    }
  }
}
